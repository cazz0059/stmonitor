package monitor.examples.tests.protocols.recur

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "recur.st", synthMonFile = false, synthProtocolFile = false)
  }
}