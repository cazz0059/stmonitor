package monitor.examples.tests.protocols.htmlReqRes

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "htmlReqRes.st", synthMonFile = false, synthProtocolFile = false)
  }
}