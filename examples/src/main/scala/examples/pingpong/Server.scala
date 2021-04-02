package examples.pingpong

import lchannels.{HttpServerIn, HttpServerManager, HttpServerOut, In}

import scala.concurrent.duration.Duration
import java.net.{ServerSocket, Socket}
import scala.collection.mutable

class Server(pinger: In[ExternalChoice1])(implicit timeout: Duration) extends Runnable {
  override def run(): Unit = {
    var resp = pinger
    var exit = false
    while(!exit) {
      resp ? {
        case ping @ Ping() =>
          resp = ping.cont !! Pong()
        case quit @ Quit() =>
          println("Quitting")
          exit = true
      }
    }
  }
}


object ServerWrapper {
  import rawhttp.core.RawHttpRequest
  val timeout = Duration.Inf

  val sessions = mutable.Map[String, HttpServerManager]()

  class PingPongManager(sessionId: String) extends HttpServerManager() {
    override def request(r: RawHttpRequest): Any = {
      if (r.getUri().getPath().equals("/ping")) {
        Ping()(HttpServerOut[Pong](this))
      } else if (r.getUri().getPath().equals("/quit")) {
        sendResponse("HTTP/1.1 200 OK\n" +
          "Content-Type: text/plain\n" +
          "Content-Length: 0" +
          "\n")
        finalize()
        Quit()
      } else {
        throw new RuntimeException("Unsupported HTTP request to: ${request.getUri()}")
      }
    }

    override def response(x: Any): String = x match {
      case _: Pong => {
        "HTTP/1.1 200 OK\n" +
          "Content-Type: text/plain\n" +
          "Content-Length: 4\n" +
          "\n" +
          "pong"
      }
      case _ => {
        finalize()
        throw new IllegalArgumentException("Unsupported message: ${x}")
      }
    }

    final override def finalize() = sessions.synchronized {
      sessions.remove(sessionId)
      super.finalize()
    }
  }

  def main(args: Array[String]): Unit = {
    val s = new ServerSocket(8080)
    println("[Ponger] Ponger started; to terminate press CTRL+c")
    while (true) {
      val client = s.accept()
      val t = new Thread { override def run(): Unit = handler(client) }
      t.start()
    }
  }

  def handler(client: Socket): Unit = {
    val http = new rawhttp.core.RawHttp()
    val request = http.parseRequest(client.getInputStream)
    val sessionIds = request.getHeaders.get("X-Session-Id")
    if (sessionIds.size() == 0) {
      http.parseResponse("HTTP/1.1 500 Internal Server Error\n" +
                          "Content-Type: text/plain\n" +
                          "Content-Length: 18\n" +
                          "\n" +
                          "Invalid session id").writeTo(client.getOutputStream)
      client.close()
      return
    } else {
      val sid = sessionIds.get(0)
      var manager: Option[PingPongManager] = None
      sessions.synchronized {
        if (sessions.keySet.contains(sid)) {
          // A server for this session is already running
          sessions(sid).queueRequest(request, client)
        } else {
          // There is no server for this session, we create one
          val mgr = new PingPongManager(sid)
          manager = Some(mgr)
          mgr.queueRequest(request, client)
          sessions(sid) = mgr
        }
      }
      if (manager.isDefined) {
        val sPinger = HttpServerIn[ExternalChoice1](manager.get)
        val server = new Server(sPinger)(timeout)
        server.run()
      }
    }
  }
}
