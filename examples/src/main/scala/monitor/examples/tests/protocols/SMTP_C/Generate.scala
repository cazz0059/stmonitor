package monitor.examples.tests.protocols.SMTP_C

import monitor.Synth

object Generate {
  def main(args: Array[String]): Unit = {
    val synth = new Synth()
    synth.apply(args(0), "smtp_c.st", synthMonFile = false, synthProtocolFile = false)
  }
}