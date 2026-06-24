//> using scala "3.8.4"
//> using jvm 21
//> using dep com.lihaoyi::cask:0.11.3
//> using dep com.lihaoyi::upickle:4.0.2
//> using dep com.lihaoyi::os-lib:0.11.3

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

  def generate(model: String, prompt: String): String =
    val payload =
      ujson.Obj(
        "model" -> model,
        "prompt" -> prompt,
        "stream" -> false
      )

    os.proc(
      "curl",
      "-s",
      "-X", "POST",
      s"$base/api/generate",
      "-H", "Content-Type: application/json",
      "--data", payload.render()
    ).call().out.text()

/**
 * =========================
 * HTTP Router Service
 * =========================
 */

object Router extends cask.MainRoutes:

  println("Bjornyx Router starting...")
  println("http://0.0.0.0:8080")
  println("backend: Ollama :11434")


  var cachedModels: List[String] = Nil

  def refresh(): List[String] =
    cachedModels = OllamaClient.listModels()
    cachedModels

  @cask.get("/health")
  def health() =
    ujson.Obj("status" -> "ok", "service" -> "bjornyx-router")

  @cask.get("/models")
  def models() =
    val m = if cachedModels.isEmpty then refresh() else cachedModels
    ujson.Obj("models" -> m)

  @cask.postJson("/generate")
  def generate(req: ujson.Value) =
    val parsed = read[GenerateRequest](req.render())

    val models = if cachedModels.isEmpty then refresh() else cachedModels

    val model =
      parsed.model.getOrElse(
        ModelRouter.pickModel(parsed.prompt, models)
      )

    val result =
      OllamaClient.generate(model, parsed.prompt)

    ujson.Obj(
      "model" -> model,
      "response" -> result
    )

  initialize()

