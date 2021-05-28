package monitor.model

import scala.collection.mutable

class Scope(val name: String, val id: String, val parentScope: Scope) {
  var isUnique: Boolean = true
  var recVariables = new mutable.HashMap[String, Statement]
  var variables = new mutable.HashMap[String, (Boolean, String)] // name => (global, type)

  var trace : List[(String, String)] = List()
  var assertions : List[String] = List()//: CNF = CNF(Set()) // the top (head) one is the most recent
  //var assertions : Set[Set[(String, Boolean)]] = Set() // false is 'not' not present, true if 'not' present

  def getAssertions: List[String] = {
    trace.map(event=>event._2)
  }
}