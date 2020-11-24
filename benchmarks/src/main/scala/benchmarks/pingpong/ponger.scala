package benchmarks.pingpong

import lchannels.In

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

class ponger(Pinger: In[Ping])(implicit ec: ExecutionContext, timeout: Duration) extends Runnable {
  override def run(): Unit = {
    println("[Ponger] Ponger started, to terminate press CTRL+c")
    var resp = Pinger
    while(true) {
      resp ? {
        case ping @ Ping() =>
          println("[Ponger] Received Ping()")
          println("[Ponger] Sending Pong()")
          resp = ping.cont !! Pong()
      }
    }
  }
}