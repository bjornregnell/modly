//> using scala "3.8.4"
//> using jvm 21
//> using dep com.lihaoyi::cask:0.11.3
//> using dep com.lihaoyi::upickle:4.0.2
//> using dep com.lihaoyi::os-lib:0.11.3

import java.util.concurrent.Semaphore

import upickle.default.{ReadWriter, macroRW, read}

/**
 * =========================
 * API Models
 * =========================
 */

case class GenerateRequest(
  prompt: String,
  model: Option[String] = None,
  temperature: Option[Double] = None
)

object GenerateRequest {
  implicit val rw: ReadWriter[GenerateRequest] = macroRW
}

case class OllamaTag(name: String)
case class OllamaTagsResponse(models: List[OllamaTag])

object OllamaTagsResponse {
  implicit val tagRW: ReadWriter[OllamaTag] = macroRW
  implicit val rw: ReadWriter[OllamaTagsResponse] = macroRW
}

/**
 * =========================
 * Router Logic
 * =========================
 */

object ModelRouter:

  def pickModel(prompt: String, available: List[String]): String =
    val p = prompt.toLowerCase

    def has(name: String) = available.contains(name)

    if (p.contains("code") || p.contains("scala") || p.contains("function")) && has("codellama")
      then "codellama"
    else if prompt.length < 40 && has("phi3")
      then "phi3"
    else if has("llama3.1")
      then "llama3.1"
    else available.headOption.getOrElse("llama3")

/**
 * =========================
 * Ollama Client (HTTP via curl using os-lib)
 * =========================
 */

object OllamaClient:

  private val base = "http://localhost:11434"

  def listModels(): List[String] =
    try
      val json =
        os.proc("curl", "-s", s"$base/api/tags")
          .call()
          .out
          .text()

      val parsed = read[OllamaTagsResponse](json)
      parsed.models.map(_.name)
    catch
      case e: Exception =>
        println(s"[ollama error] ${e.getMessage}")
        Nil

  private def postGenerate(payload: ujson.Obj): String =
    os.proc(
      "curl",
      "-s",
      "-X", "POST",
      s"$base/api/generate",
      "-H", "Content-Type: application/json",
      "--data", payload.render()
    ).call().out.text()

  def generate(model: String, prompt: String, temperature: Option[Double]): String =
    val payload =
      ujson.Obj(
        "model" -> model,
        "prompt" -> prompt,
        "stream" -> false
      )

    temperature.foreach: t =>
      payload("options") = ujson.Obj("temperature" -> t)

    postGenerate(payload)

  def loadModel(model: String): String =
    postGenerate(ujson.Obj("model" -> model, "stream" -> false))

  def unloadModel(model: String): String =
    postGenerate(ujson.Obj("model" -> model, "keep_alive" -> 0, "stream" -> false))

/**
 * =========================
 * HTTP Router Service
 * =========================
 */

object Router extends cask.MainRoutes:

  println("Bjornyx Router starting...")
  println("http://0.0.0.0:8080")
  println("backend: Ollama :11434")


  private val singleFlight = Semaphore(1)
  private val helpMarkdown =
    """# Bjornyx Router
      |
      |Purpose:
      |This server proxies requests to a local Ollama instance and manages one active model at a time.
      |It is designed for single-flight operation so generation and model switching do not overlap.
      |
      |## Endpoints
      |
      |### `GET /health`
      |Returns a basic health check.
      |
      |Example:
      |```bash
      |curl -s http://localhost:8080/health
      |```
      |
      |Response:
      |```json
      |{"status":"ok","service":"bjornyx-router"}
      |```
      |
      |### `GET /models`
      |Lists the available Ollama models and the current active model.
      |
      |Response fields:
      |- `models`: the models reported by Ollama
      |- `active_model`: the currently selected model, or `null`
      |
      |Example:
      |```bash
      |curl -s http://localhost:8080/models
      |```
      |
      |Response:
      |```json
      |{"models":["gemma3:latest"],"active_model":"gemma3:latest"}
      |```
      |
      |### `POST /set-model`
      |Selects the active model explicitly.
      |The `model` value must be one of the names returned by `GET /models`.
      |
      |Request body:
      |```json
      |{"model":"gemma3:latest"}
      |```
      |
      |Behavior:
      |- If the requested model is unknown, the server returns `400`.
      |- If a different model is already active, the server unloads it through Ollama and loads the new one.
      |- If the same model is already active, the request is a no-op.
      |
      |Example:
      |```bash
      |curl -s -X POST http://localhost:8080/set-model -H "Content-Type: application/json" --data '{"model":"gemma3:latest"}'
      |```
      |
      |Success response:
      |```json
      |{"model":"gemma3:latest","previous_model":null,"changed":true}
      |```
      |
      |Error response:
      |```json
      |{"error":"unknown model: not-a-real-model","models":["gemma3:latest"]}
      |```
      |
      |### `POST /generate`
      |Generates a response using the current active model.
      |If no model is active yet, the router picks one from the available models using the prompt.
      |
      |Request body:
      |```json
      |{"prompt":"hello","temperature":0.1}
      |```
      |
      |Parameters:
      |- `prompt` required string
      |- `temperature` optional number
      |
      |Example:
      |```bash
      |curl -s -X POST http://localhost:8080/generate -H "Content-Type: application/json" --data '{"prompt":"hello","temperature":0.1}'
      |```
      |
      |Response:
      |```json
      |{"model":"gemma3:latest","response":"..."}
      |```
      |
      |## Typical flow
      |1. Call `GET /models`.
      |2. Call `POST /set-model` with one of those model names.
      |3. Call `POST /generate` as needed.
      |""".stripMargin
  private var cachedModels: List[String] = Nil
  private var activeModel: Option[String] = None

  private def withSingleFlight[T](body: => T): T =
    singleFlight.acquire()
    try body
    finally singleFlight.release()

  private def refresh(): List[String] =
    cachedModels = OllamaClient.listModels()
    cachedModels

  private def availableModels: List[String] =
    if cachedModels.isEmpty then refresh() else cachedModels

  private def modelJson(model: Option[String]): ujson.Value =
    model match
      case Some(value) => ujson.Str(value)
      case None => ujson.Null

  private def ensureActiveModel(prompt: String): String =
    activeModel.getOrElse:
      val selected = ModelRouter.pickModel(prompt, availableModels)
      OllamaClient.loadModel(selected)
      activeModel = Some(selected)
      selected

  @cask.get("/health")
  def health() =
    ujson.Obj("status" -> "ok", "service" -> "bjornyx-router")

  @cask.get("/help")
  def help() =
    ujson.Obj("help" -> helpMarkdown)

  @cask.get("/models")
  def models() =
    ujson.Obj(
      "models" -> availableModels,
      "active_model" -> modelJson(activeModel)
    )

  @cask.postJson("/set-model")
  def setModel(model: String) =
    withSingleFlight:
      val models = availableModels

      if !models.contains(model) then
        cask.Response(
          ujson.Obj("error" -> ("unknown model: " + model), "models" -> models),
          statusCode = 400
        )
      else
        val previous = activeModel

        if !previous.contains(model) then
          previous.foreach(OllamaClient.unloadModel)
          OllamaClient.loadModel(model)
          activeModel = Some(model)

        cask.Response(
          ujson.Obj(
            "model" -> model,
            "previous_model" -> modelJson(previous),
            "changed" -> !previous.contains(model)
          )
        )

  @cask.postJson("/generate")
  def generate(prompt: String, temperature: Option[Double] = None) =
    withSingleFlight:
      val selectedModel = ensureActiveModel(prompt)
      val result = OllamaClient.generate(selectedModel, prompt, temperature)

      ujson.Obj(
        "model" -> selectedModel,
        "response" -> result
      )

  initialize()

