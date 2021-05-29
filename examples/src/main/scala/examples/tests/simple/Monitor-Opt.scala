package examples.tests.simple
import lchannels.{In, Out}
import monitor.util.ConnectionManager
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.control.TailCalls.{TailRec, done, tailcall}
class Monitor(external: ConnectionManager, internal: In[Get], max: Int, report: String => Unit)(implicit ec: ExecutionContext, timeout: Duration) extends Runnable {
	object payloads {
		object Get_3 {
			var ans: Boolean = _
		}
		object Confirm_1 {
			var msg: String = _
		}
	}
	override def run(): Unit = {
		report("[MONITOR] Monitor started, setting up connection manager")
		external.setup()
		sendGet_3(internal, external, 0).result
    external.close()
  }
	def sendGet_3(internal: In[Get], external: ConnectionManager, count: Int): TailRec[Unit] = {
		internal ? {
			case msg @ Get(_) =>
				if(msg.ans && util.redundant()){
					external.send(msg)
			payloads.Get_3.ans = msg.ans
					if (count < max) {
						receiveInternalChoice1(msg.cont, external, count+1)
					} else { tailcall(receiveInternalChoice1(msg.cont, external, 0)) }
				} else {
				report("[MONITOR] VIOLATION in Assertion: ans && util.redundant()"); done() }
			case msg @ _ => report(f"[MONITOR] VIOLATION unknown message: $msg"); done()
		}
	}
	def receiveInternalChoice1(internal: Out[InternalChoice1], external: ConnectionManager, count: Int): TailRec[Unit] = {
		external.receive() match {
			case msg @ Confirm(_)=>
				if(payloads.Get_3.ans){
					internal ! msg; done()
				} else {
				report("[MONITOR] VIOLATION in Assertion: ans"); done() }
			case msg @ _ => report(f"[MONITOR] VIOLATION unknown message: $msg"); done()
		}
	}
}