package monitor.examples.tests.first

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "first.st", synthMonFile = false, synthProtocolFile = false)
  }
}