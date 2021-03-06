package monitor.model

case class SendStatement(label: String, types: Map[String, String], condition: Expression, continuation: Statement) extends Statement
