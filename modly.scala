//> using scala "3.8.4"
//> using jvm 21
//> using dep com.lihaoyi::cask:0.11.3
//> using dep com.lihaoyi::upickle:4.4.3

// NO os-lib, and deliberately no HTTP library either: modly used to reach Ollama by shelling out to
// `curl` through os.proc, which made every backend call depend on the JVM's ability to FORK. On
// 2026-08-27 the JDK was updated in place under the running server, so `jspawnhelper` no longer
// matched the live JVM and ProcessBuilder.start() failed for every call — for five weeks, while
// /health still answered "ok". The JDK's own HttpClient needs no subprocess and no new dependency,
// so that entire failure class is gone rather than merely reported.

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
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
 * HTTP (JDK client, no subprocess) + Ollama Client
 * =========================
 */

object Http:

  // One client for the process. connectTimeout guards a host that is DOWN (fail fast); the per-request
  // timeout guards one that is merely SLOW. Both are needed, and they want very different values.
  private val client: HttpClient =
    HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(5)).build()

  def get(url: String, timeout: Duration): Either[String, String] =
    send(HttpRequest.newBuilder(URI.create(url)).timeout(timeout).GET().build())

  def postJson(url: String, body: String, timeout: Duration): Either[String, String] =
    send(HttpRequest.newBuilder(URI.create(url))
      .timeout(timeout)
      .header("Content-Type", "application/json")
      .POST(HttpRequest.BodyPublishers.ofString(body))
      .build())

  /** Left carries a REASON. Nothing here ever turns a failure into an empty success — that conversion
    * is exactly what hid a five-week outage behind `{"models":[]}`. A non-2xx is a failure too: `curl -s`
    * used to hand back an error body as if it were data. */
  private def send(req: HttpRequest): Either[String, String] =
    try
      val res = client.send(req, HttpResponse.BodyHandlers.ofString())
      if res.statusCode() / 100 == 2 then Right(res.body())
      else Left(s"${req.method()} ${req.uri()} -> HTTP ${res.statusCode()}")
    catch case e: Throwable =>
      Left(s"${req.method()} ${req.uri()} -> ${e.getClass.getSimpleName}: ${e.getMessage}")

object OllamaClient:

  private val base = OllamaBase

  // Listing is cheap and on the health path; generating can take minutes on a cold model load, so the
  // two get very different budgets. The generate budget must EXCEED any client's own read timeout —
  // introprog's autotranslator waits 120 s — or modly gives up first and looks like the culprit.
  private val ListTimeout = Duration.ofSeconds(10)
  private val GenerateTimeout = Duration.ofMinutes(10)

  /** Models Ollama reports. `Left` = could not ASK (unreachable, non-2xx, unparseable);
    * `Right(Nil)` = asked fine, there are none. Collapsing those two into one empty list is what made
    * a dead backend indistinguishable from an empty one for every caller downstream. */
  def listModels(): Either[String, List[String]] =
    Http.get(s"$base/api/tags", ListTimeout).flatMap: json =>
      try Right(read[OllamaTagsResponse](json).models.map(_.name))
      catch case e: Throwable => Left(s"unparseable /api/tags: ${e.getClass.getSimpleName}")

  private def postGenerate(payload: ujson.Obj): Either[String, String] =
    Http.postJson(s"$base/api/generate", payload.render(), GenerateTimeout)

  /** Generate with optional temperature AND seed (both passed through to Ollama options
    * so callers can get deterministic output: temperature=0 + a fixed seed). */
  def generate(model: String, prompt: String, temperature: Option[Double], seed: Option[Int]): Either[String, String] =
    val payload = ujson.Obj("model" -> model, "prompt" -> prompt, "stream" -> false)
    val opts = ujson.Obj()
    temperature.foreach(t => opts("temperature") = t)
    seed.foreach(s => opts("seed") = s)
    if opts.value.nonEmpty then payload("options") = opts
    postGenerate(payload).map: raw =>
      // Ollama returns a JSON object; extract just the generated text (was returning the whole blob).
      try ujson.read(raw)("response").str catch case _: Throwable => raw

  def loadModel(model: String): Either[String, String] =
    postGenerate(ujson.Obj("model" -> model, "stream" -> false))

  def unloadModel(model: String): Either[String, String] =
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
    |PROBES the Ollama backend and reports what it found — it does not merely say the server is running.
    |Response fields: `status` (`ok`/`degraded`), `ollama_reachable`, `model_count` or `error`,
    |plus `started_at` + `uptime_seconds` so a process older than its own JDK is visible without ssh.
    |- Backend unreachable -> `503` with `status: degraded` and the reason.
    |```bash
    |curl -s http://localhost:$Port/health
    |```
    |
    |### `GET /models`
    |Lists the available Ollama models (queried LIVE) and the current active model + active params.
    |Response fields: `models`, `ollama_reachable`, `active_model` (or null), `temperature`, `seed`.
    |- Backend unreachable -> `503` with `error` and `ollama_reachable: false`, NEVER a `200` with an
    |  empty list: a caller must be able to tell a dead backend from an empty one.
    |```bash
    |curl -s http://localhost:$Port/models
    |```
    |
    |### `POST /set-model`
    |Selects the active model and (optionally) sets default generation params reused by /generate.
    |Request body: `{"model":"qwen2.5:7b","temperature":0,"seed":42}` (temperature/seed optional).
    |- Unknown model -> `400`. Backend unreachable -> `503` (NOT 400: we never got to ask).
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
    |- Backend unreachable / no model -> `503`; Ollama refused the generation -> `502` with `error`.
    |  A failed generation is never returned as if it were generated text.
    |```bash
    |curl -s -X POST http://localhost:$Port/generate -H "Content-Type: application/json" \\
    |  --data '{"prompt":"hello","temperature":0,"seed":42}'
    |```
    |
    |## Typical flow
    |1. `GET /models`  2. `POST /set-model` (with model + temperature + seed)  3. `POST /generate`
    |""".stripMargin

  // Today's outage was invisible partly because nothing reported HOW LONG this process had been up:
  // the running JVM was five weeks older than the JDK underneath it, and only `ps -o lstart=` said so.
  // /health now carries it, so "restart it" is a decision anyone can reach without ssh.
  private val startedAt: java.time.Instant = java.time.Instant.now()

  private var activeModel: Option[String] = None
  private var activeTemperature: Option[Double] = None
  private var activeSeed: Option[Int] = None

  private def withSingleFlight[T](body: => T): T =
    singleFlight.acquire()
    try body
    finally singleFlight.release()

  /** Query Ollama LIVE every call so newly-pulled models appear immediately (and /set-model
    * validates against the current set). The `Left` is propagated rather than flattened to Nil, so a
    * caller can REFUSE on an unreachable backend instead of acting on an empty set as if it were an
    * answer. (The old `cachedModels` var is gone: nothing read it, and a stale last-known-good list is
    * the same lie in slower form.) */
  private def availableModels: Either[String, List[String]] = OllamaClient.listModels()

  private def strOrNull(s: Option[String]): ujson.Value = s.map(ujson.Str(_)).getOrElse(ujson.Null)
  private def numOrNull(d: Option[Double]): ujson.Value = d.map(ujson.Num(_)).getOrElse(ujson.Null)

  private def ensureActiveModel(prompt: String): Either[String, String] =
    activeModel match
      case Some(m) => Right(m)
      case None =>
        availableModels.flatMap: ms =>
          if ms.isEmpty then Left("Ollama is reachable but reports no models")
          else
            val selected = ModelRouter.pickModel(prompt, ms)
            // A load failure must NOT leave activeModel set: that would make every later /generate
            // report a model this server never actually loaded.
            OllamaClient.loadModel(selected).map: _ =>
              activeModel = Some(selected)
              selected

  /** A health check that cannot fail is not a health check. This one answered `{"status":"ok"}` for
    * five weeks while EVERY backend call was failing, which is why the outage went unnoticed. It now
    * reports the backend probe it just made, and 503s when that probe fails, so anything watching it
    * sees the outage instead of the server's opinion of itself. */
  @cask.get("/health")
  def health() =
    val uptime = java.time.Duration.between(startedAt, java.time.Instant.now())
    val base = ujson.Obj(
      "service" -> "bjornyx-router",
      "started_at" -> startedAt.toString,
      "uptime_seconds" -> uptime.toSeconds.toDouble
    )
    OllamaClient.listModels() match
      case Right(ms) =>
        base("status") = "ok"; base("ollama_reachable") = true; base("model_count") = ms.size
        cask.Response(base)
      case Left(err) =>
        base("status") = "degraded"; base("ollama_reachable") = false; base("error") = err
        cask.Response(base, statusCode = 503)

  @cask.get("/help")
  def help() =
    ujson.Obj("help" -> helpMarkdown)

  @cask.get("/models")
  def models() =
    def body(models: ujson.Value) = ujson.Obj(
      "models" -> models,
      "active_model" -> strOrNull(activeModel),
      "temperature" -> numOrNull(activeTemperature),
      "seed" -> numOrNull(activeSeed.map(_.toDouble))
    )
    availableModels match
      case Right(ms) =>
        val o = body(ms); o("ollama_reachable") = true
        cask.Response(o)
      case Left(err) =>
        // 503 + an explicit flag, NOT a 200 with an empty array. Clients that only look at the array
        // (introprog's autotranslator did) then correctly read this as "modly down" and fall through to
        // their own backend, instead of reporting "UP (0 models)" and silently degrading to no-model.
        val o = body(ujson.Arr()); o("ollama_reachable") = false; o("error") = err
        cask.Response(o, statusCode = 503)

  @cask.postJson("/set-model")
  def setModel(model: String, temperature: Option[Double] = None, seed: Option[Int] = None) =
    withSingleFlight:
      availableModels match
      // NOT "unknown model": we never got to ask. A 400 here blames the caller for a backend outage,
      // and that misdiagnosis is expensive — it sends people looking at their request instead of the box.
      case Left(err) =>
        cask.Response(ujson.Obj("error" -> ("cannot reach Ollama: " + err), "ollama_reachable" -> false),
          statusCode = 503)
      case Right(models) if !models.contains(model) =>
        cask.Response(
          ujson.Obj("error" -> ("unknown model: " + model), "models" -> models),
          statusCode = 400
        )
      case Right(_) =>
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
      val effTemperature = temperature.orElse(activeTemperature) // per-request overrides session default
      val effSeed = seed.orElse(activeSeed)
      // A backend failure is reported as a failure. Previously a broken curl threw out of here and cask
      // turned it into a bare 500 with no reason; worse, a non-2xx body came back as if it were the
      // generated TEXT, so a caller could cache an Ollama error message as a translation.
      ensureActiveModel(prompt) match
        case Left(err) =>
          cask.Response(ujson.Obj("error" -> err, "ollama_reachable" -> false), statusCode = 503)
        case Right(selectedModel) =>
          OllamaClient.generate(selectedModel, prompt, effTemperature, effSeed) match
            case Left(err) =>
              cask.Response(ujson.Obj("error" -> err, "model" -> selectedModel), statusCode = 502)
            case Right(result) =>
              cask.Response(ujson.Obj(
                "model" -> selectedModel,
                "temperature" -> numOrNull(effTemperature),
                "seed" -> numOrNull(effSeed.map(_.toDouble)),
                "response" -> result
              ))

  initialize()
