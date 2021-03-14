package monitor.examples.tests.simple

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "simple.st", synthMonFile = false, synthProtocolFile = false)
  }
}