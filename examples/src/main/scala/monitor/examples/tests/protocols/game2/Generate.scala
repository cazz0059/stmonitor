package monitor.examples.tests.protocols.game2

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "game2.st", synthMonFile = false, synthProtocolFile = false)
  }
}
