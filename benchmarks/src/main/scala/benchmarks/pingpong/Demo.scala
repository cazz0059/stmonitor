package benchmarks.pingpong

import akka.actor.{ActorSystem, Props}
import lchannels.LocalChannel

object Demo extends App{
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._
  val timeout = Duration.Inf

  val system = ActorSystem("System")

  val (in, out) = LocalChannel.factory[Ping]()
  val Mon = system.actorOf(Props(new Mon(out)(global, timeout)), name="Mon")
  val ponger = new ponger(in)(global, timeout)

  Mon ! MonStart

  val pongerThread = new Thread {
    override def run(): Unit = {
      ponger.run()
    }
  }

  pongerThread.start()
}
