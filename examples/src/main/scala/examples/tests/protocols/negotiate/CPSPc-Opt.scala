package examples.tests.protocols.negotiate

import lchannels.Out
case class Propose1(proposal: String, i: Int)(val cont: Out[ExternalChoice1])
sealed abstract class ExternalChoice1
case class Propose2(proposal2: String, i2: Int)(val cont: Out[InternalChoice1]) extends ExternalChoice1
sealed abstract class InternalChoice1
case class Reject2() extends InternalChoice1
case class Propose3(proposal3: String, i3: Int)(val cont: Out[ExternalChoice1]) extends InternalChoice1
