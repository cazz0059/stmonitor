package monitor.model

import scala.collection.mutable

class Scope(val name: String, val parentScope: Scope) {
  var recVariables = new mutable.HashMap[String, Statement] // varName => corresp statement im guessing
  var variables = new mutable.HashMap[String, (Boolean, String)] // name => (global, type)

  var assertions : Set[Set[(String, Boolean)]] = Set() // false is 'not' not present, true if 'not' present
}