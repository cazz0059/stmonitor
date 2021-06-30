package monitor.model

import scala.collection.mutable

class Scope(val name: String, val id: String, val parentScope: Scope) {
  var isUnique: Boolean = true
  var recVariables = new mutable.HashMap[String, Statement]
  var variables = new mutable.HashMap[String, (Boolean, String)] // name => (global, type)

  var trace : List[(String, String)] = List()
  var assertions : List[String] = List()

  def getAssertions: List[String] = {
    trace.map(event=>event._2)
  }
}