package examples.tests.simple
import lchannels.Out
case class Get(ans: Boolean)(val cont: Out[InternalChoice1])
sealed abstract class InternalChoice1
case class Confirm(msg: String) extends InternalChoice1
