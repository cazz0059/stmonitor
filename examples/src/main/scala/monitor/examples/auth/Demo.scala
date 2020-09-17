package monitor.examples.auth

import akka.actor.{ActorSystem, Props}
import lchannels.LocalChannel
import monitor.examples.auth.{Auth}

object Demo extends App{
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  val timeout = Duration.Inf

  val system = ActorSystem("System")

  val (in, out) = LocalChannel.factory[Auth]()
  val Mon = system.actorOf(Props(new Mon(out)(global, timeout)), name="Mon")
  val server = new Server(in)(global, timeout)

  Mon ! MonStart

  val serverThread = new Thread {
    override def run(): Unit = {
      server.run()
    }
  }

  serverThread.start()
}
