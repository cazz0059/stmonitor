package monitor.examples.tests.protocols.arithmetic

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "arithmetic.st", synthMonFile = false, synthProtocolFile = false)
  }
}