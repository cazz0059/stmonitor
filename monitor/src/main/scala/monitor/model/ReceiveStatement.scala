package monitor.model

case class ReceiveStatement(label: String, types: Map[String, String], condition: Expression, continuation: Statement) extends Statement