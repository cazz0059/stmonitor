package monitor.examples.tests.protocols.negotiate

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "negotiate.st", synthMonFile = false, synthProtocolFile = false)
  }
}
