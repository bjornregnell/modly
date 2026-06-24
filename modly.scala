//> using scala "3.8.4"
//> using jvm 21
//> using dep com.lihaoyi::cask:0.11.3
//> using dep com.lihaoyi::upickle:4.4.3
//> using dep com.lihaoyi::os-lib:0.11.8

import java.util.concurrent.Semaphore

import upickle.default.{ReadWriter, macroRW, read}

/**
 * =========================
 * Config (single source of truth — no hard-baked ports/hosts in strings)
 * =========================
 */

val Host = "0.0.0.0"   // bind all interfaces so the LAN can reach modly (cask default is localhost)
val Port = 8080        // modly HTTP port
val OllamaPort = 11434 // backend Ollama port
val OllamaBase = s"http://localhost:$OllamaPort"

/**
 * =========================
 * API Models
 * =========================
 */

case class GenerateRequest(
  prompt: String,
  model: Option[String] = None,
  temperature: Option[Double] = None,
  seed: Option[Int] = None
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

  /** Fallback model pick — only used by /generate if no model was set via /set-model. */
  def pickModel(prompt: String, available: List[String]): String =
    available.headOption.getOrElse("llama3")

/**
 * =========================
 * Ollama Client (HTTP via curl using os-lib)
 * =========================
 */

object OllamaClient:

  private val base = OllamaBase

  def listModels(): List[String] =
    try
      val json = os.proc("curl", "-s", s"$base/api/tags").call().out.text()
      read[OllamaTagsResponse](json).models.map(_.name)
    catch
      case e: Exception =>
        println(s"[ollama error] ${e.getMessage}")
        Nil

  private def postGenerate(payload: ujson.Obj): String =
    os.proc(
      "curl", "-s", "-X", "POST",
      s"$base/api/generate",
      "-H", "Content-Type: application/json",
      "--data", payload.render()
    ).call().out.text()

  /** Generate with optional temperature AND seed (both passed through to Ollama options
    * so callers can get deterministic output: temperature=0 + a fixed seed). */
  def generate(model: String, prompt: String, temperature: Option[Double], seed: Option[Int]): String =
    val payload = ujson.Obj("model" -> model, "prompt" -> prompt, "stream" -> false)
    val opts = ujson.Obj()
    temperature.foreach(t => opts("temperature") = t)
    seed.foreach(s => opts("seed") = s)
    if opts.value.nonEmpty then payload("options") = opts
    val raw = postGenerate(payload)
    // Ollama returns a JSON object; extract just the generated text (was returning the whole blob).
    try ujson.read(raw)("response").str catch case _: Throwable => raw

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

  override def host: String = Host
  override def port: Int = Port

  println("Bjornyx Router starting...")
  println(s"http://$Host:$Port")
  println(s"backend: Ollama :$OllamaPort")

  private val singleFlight = Semaphore(1)

  private val helpMarkdown = s"""# Bjornyx Router
    |
    |Purpose:
    |This server proxies requests to a local Ollama instance and manages one active model at a time.
    |It is designed for single-flight operation so generation and model switching do not overlap.
    |
    |## Endpoints
    |
    |### `GET /health`
    |Returns a basic health check.
    |```bash
    |curl -s http://localhost:$Port/health
    |```
    |
    |### `GET /models`
    |Lists the available Ollama models (queried LIVE) and the current active model + active params.
    |Response fields: `models`, `active_model` (or null), `temperature`, `seed`.
    |```bash
    |curl -s http://localhost:$Port/models
    |```
    |
    |### `POST /set-model`
    |Selects the active model and (optionally) sets default generation params reused by /generate.
    |Request body: `{"model":"qwen2.5:7b","temperature":0,"seed":42}` (temperature/seed optional).
    |- Unknown model -> `400`.
    |- Switching models unloads the previous one and loads the new one through Ollama.
    |```bash
    |curl -s -X POST http://localhost:$Port/set-model -H "Content-Type: application/json" \\
    |  --data '{"model":"qwen2.5:7b","temperature":0,"seed":42}'
    |```
    |
    |### `POST /generate`
    |Generates using the active model. `temperature`/`seed` may be given per request; otherwise the
    |defaults set via /set-model are used. For reproducible output pass temperature=0 and a fixed seed.
    |Parameters: `prompt` (required), `temperature` (optional number), `seed` (optional integer).
    |```bash
    |curl -s -X POST http://localhost:$Port/generate -H "Content-Type: application/json" \\
    |  --data '{"prompt":"hello","temperature":0,"seed":42}'
    |```
    |
    |## Typical flow
    |1. `GET /models`  2. `POST /set-model` (with model + temperature + seed)  3. `POST /generate`
    |""".stripMargin

  private var cachedModels: List[String] = Nil
  private var activeModel: Option[String] = None
  private var activeTemperature: Option[Double] = None
  private var activeSeed: Option[Int] = None

  private def withSingleFlight[T](body: => T): T =
    singleFlight.acquire()
    try body
    finally singleFlight.release()

  /** Query Ollama LIVE every call so newly-pulled models appear immediately (and /set-model
    * validates against the current set). */
  private def availableModels: List[String] =
    cachedModels = OllamaClient.listModels()
    cachedModels

  private def strOrNull(s: Option[String]): ujson.Value = s.map(ujson.Str(_)).getOrElse(ujson.Null)
  private def numOrNull(d: Option[Double]): ujson.Value = d.map(ujson.Num(_)).getOrElse(ujson.Null)

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
      "active_model" -> strOrNull(activeModel),
      "temperature" -> numOrNull(activeTemperature),
      "seed" -> numOrNull(activeSeed.map(_.toDouble))
    )

  @cask.postJson("/set-model")
  def setModel(model: String, temperature: Option[Double] = None, seed: Option[Int] = None) =
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

        // store session-default generation params (used by /generate when not overridden per-request)
        temperature.foreach(t => activeTemperature = Some(t))
        seed.foreach(s => activeSeed = Some(s))

        cask.Response(
          ujson.Obj(
            "model" -> model,
            "previous_model" -> strOrNull(previous),
            "changed" -> !previous.contains(model),
            "temperature" -> numOrNull(activeTemperature),
            "seed" -> numOrNull(activeSeed.map(_.toDouble))
          )
        )

  @cask.postJson("/generate")
  def generate(prompt: String, temperature: Option[Double] = None, seed: Option[Int] = None) =
    withSingleFlight:
      val selectedModel = ensureActiveModel(prompt)
      val effTemperature = temperature.orElse(activeTemperature) // per-request overrides session default
      val effSeed = seed.orElse(activeSeed)
      val result = OllamaClient.generate(selectedModel, prompt, effTemperature, effSeed)

      ujson.Obj(
        "model" -> selectedModel,
        "temperature" -> numOrNull(effTemperature),
        "seed" -> numOrNull(effSeed.map(_.toDouble)),
        "response" -> result
      )

  initialize()
