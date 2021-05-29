package examples.tests.protocols.negotiate

import lchannels.{In, Out}
import monitor.util.ConnectionManager
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.control.TailCalls.{TailRec, done, tailcall}
class Monitor(external: ConnectionManager, internal: Out[Propose1], max: Int, report: String => Unit)(implicit ec: ExecutionContext, timeout: Duration) extends Runnable {
	object payloads {
		object Propose1_9 {
			var proposal: String = _
			var i: Int = _
		}
		object Propose2_8 {
			var proposal2: String = _
			var i2: Int = _
		}
		object Reject2_6 {
		}
		object Propose3_7 {
			var proposal3: String = _
			var i3: Int = _
		}
	}
	override def run(): Unit = {
		report("[MONITOR] Monitor started, setting up connection manager")
		external.setup()
		receivePropose1_9(internal, external, 0).result
    external.close()
  }
  def receivePropose1_9(internal: Out[Propose1], external: ConnectionManager, count: Int): TailRec[Unit] = {
		external.receive() match {
			case msg @ Propose1(_, _)=>
				if(msg.i == 0){
					val cont = internal !! Propose1(msg.proposal, msg.i)_
			payloads.Propose1_9.i = msg.i
					if (count < max) {
						sendExternalChoice1(cont, external,count+1)
					} else { tailcall(sendExternalChoice1(cont, external,0)) }
				} else {
				report("[MONITOR] VIOLATION in Assertion: i == 0"); done() }
			case msg @ _ => report(f"[MONITOR] VIOLATION unknown message: $msg"); done()
		}
	}
	def sendExternalChoice1(internal: In[ExternalChoice1], external: ConnectionManager, count: Int): TailRec[Unit] = {
		internal ? {
			case msg @ Propose2(_, _) =>
				if(payloads.Propose1_9.i == 0){
					external.send(msg)
			payloads.Propose2_8.i2 = msg.i2
					if (count < max) {
						receiveInternalChoice1(msg.cont, external, count+1)
					} else { tailcall(receiveInternalChoice1(msg.cont, external, 0)) }
				} else {
				report("[MONITOR] VIOLATION in Assertion: i == 0"); done() }
			case msg @ _ => report(f"[MONITOR] VIOLATION unknown message: $msg"); done()
		}
	}
	def receiveInternalChoice1(internal: Out[InternalChoice1], external: ConnectionManager, count: Int): TailRec[Unit] = {
		external.receive() match {
			case msg @ Reject2()=>
				if(payloads.Propose2_8.i2 < 0){
					internal ! msg; done()
				} else {
				report("[MONITOR] VIOLATION in Assertion: i2 < 0"); done() }
			case msg @ Propose3(_, _)=>
				if(payloads.Propose2_8.i2 >= 0){
					val cont = internal !! Propose3(msg.proposal3, msg.i3)_
					if (count < max) {
						sendExternalChoice1(cont, external,count+1)
					} else { tailcall(sendExternalChoice1(cont, external,0)) }
				} else {
				report("[MONITOR] VIOLATION in Assertion: i2 >= 0"); done() }
			case msg @ _ => report(f"[MONITOR] VIOLATION unknown message: $msg"); done()
		}
	}
}