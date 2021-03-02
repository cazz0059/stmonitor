package monitor.examples.tests.protocols.listRes

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "listRes.st", synthMonFile = false, synthProtocolFile = false)
  }
}