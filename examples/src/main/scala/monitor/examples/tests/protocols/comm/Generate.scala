package monitor.examples.tests.protocols.comm

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "comm.st", synthMonFile = false, synthProtocolFile = false)
  }
}
