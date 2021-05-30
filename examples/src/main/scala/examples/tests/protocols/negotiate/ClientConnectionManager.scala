package examples.tests.protocols.negotiate

import monitor.util.ConnectionManager

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}

class ClientConnectionManager(client: java.net.Socket) extends ConnectionManager {
  var outB: BufferedWriter = _
  var inB: BufferedReader = _

  private val propose1R = """PROPOSE1 (.+) (.+)""".r
  private val accept2R = """ACCEPT2 """.r
  private val reject2R = """REJECT2 """.r
  private val propose3R = """PROPOSE3 (.+)""".r

  def setup(): Unit = {
    outB = new BufferedWriter(new OutputStreamWriter(client.getOutputStream))
    inB = new BufferedReader(new InputStreamReader(client.getInputStream))
  }

  def receive(): Any = inB.readLine() match {
    case propose1R(proposal1, i) => Propose1(proposal1, i.toInt)(null);
    //case accept2R() => Accept2();
    case reject2R() => Reject2();
    case e => e
  }

  def send(x: Any): Unit = x match {
    //case Accept1() => outB.write(f"ACCEPT1\r\n"); outB.flush();
    //case Reject1() => outB.write(f"REJECT1\r\n"); outB.flush();
    case Propose2(proposal2, i2) =>
      outB.write(f"PROPOSE2 $proposal2 $i2\r\n"); outB.flush();
    case _ => close(); throw new Exception("[CM] Error: Unexpected message by Mon");
  }

  def close(): Unit = {
    outB.flush();
    inB.close();
    outB.close();
  }
}