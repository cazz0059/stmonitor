package monitor.parser

//import scala.collection.mutable
//import scala.reflect.runtime._
//import scala.reflect.runtime.universe._
//import scala.tools.reflect.ToolBox

import monitor.model._

import java.io.{File, PrintWriter}
//import monitor.model.Scope

class STSolverHelper {

  ////////////////////////////////////////////

  def conditionToString(not_factor : NotFactor): String = {
    var stringCondition = ""

    if (not_factor.t) {
      stringCondition = stringCondition ++ " not"
    }
    stringCondition = stringCondition ++ " "
    not_factor.factor match {
      case Expression(terms) =>
        stringCondition = stringCondition ++ conditionToString(Expression(terms))
      case Variable(name) =>
        stringCondition = stringCondition ++ name
    }

    stringCondition
  }

  def conditionToString(term : Term): String = {
    var stringCondition = ""

    val not_factors = term.not_factors
    if (not_factors.length > 1) {
      for (not_factor <- not_factors) {
        stringCondition = stringCondition ++ "("
        stringCondition = stringCondition ++ conditionToString(not_factor)
        stringCondition = stringCondition ++ ") && "
      }
      if (not_factors.nonEmpty) stringCondition.dropRight(3) // removing last and
    }
    else if (not_factors.length == 1){
      stringCondition = stringCondition ++ conditionToString(not_factors.head)
    }
    stringCondition
  }

  def conditionToString(expression : Expression) : String ={
    var stringCondition = ""
    val terms = expression.terms
    if (terms.length > 1) {
      for (term <- terms) {
        stringCondition = stringCondition ++ "("
        stringCondition = stringCondition ++ conditionToString(term)
        stringCondition = stringCondition ++ ") || "
      }
      if (terms.nonEmpty) stringCondition.dropRight(3) // removing last or
    }
    else if (terms.length == 1) {
      stringCondition = stringCondition ++ conditionToString(terms.head)
    }

    stringCondition
  }

  ////////////////////////////////////////////  CNF BUILDERS

  def negate(not_factor : NotFactor) : NotFactor = {
    //    not_factor.factor match {
    //      case Variable(name)
    //    }
    NotFactor(!not_factor.t, not_factor.factor)
  }

  def negate(term : Term): Term = {
    if (term.not_factors.length == 1) {
      val not_factor = NotFactor(!term.not_factors.head.t, term.not_factors.head.factor)
      Term(List(not_factor))
    }
    val not_factor = NotFactor(t = true, term.not_factors.head.factor)
    Term(List(not_factor))
  }

  def negate(expression : Expression) : Expression = { // this is never called, is it because it is not needed
    // or am I missing something?
    if (expression.terms.length == 1) {
      if (expression.terms.head.not_factors.length == 1) {
        val not_factor = NotFactor(!expression.terms.head.not_factors.head.t, expression.terms.head.not_factors.head.factor)
        val term = Term(List(not_factor))
        Expression(List(term)) // depends if the factor is negated or not
      }
    }
    val not_factor = NotFactor(t = true, expression.terms.head.not_factors.head.factor)
    val term = Term(List(not_factor))
    Expression(List(term)) // normal negation
  }

  def mergeCNF(cnf1 : CNF, cnf2 : CNF) : CNF = {
    var mergedClauses : Set[Clause] = Set()
    for (x <- cnf1.clauses.toIterator) {
      for (y <- cnf2.clauses.toIterator) {
        val mergedClause = Clause(x.literals ++ y.literals)
        mergedClauses = mergedClauses + mergedClause
//        val clause : Set[(String, Boolean)] = Set(x, y)
//        val clauseSet : Set[Set[(String, Boolean)]] = Set(clause)
//        cnf = cnf ++ clauseSet
      }
    }
    CNF(mergedClauses)
  }

  //  private def getCurrentConditions(factor : Factor) : Set[Set[(String, Boolean)]] = {
  //    factor match {
  //      case Expression(terms) =>
  //        getCurrentConditions(Expression(terms))
  //      case Variable(name) =>
  //        Set(name) // base case, change to if negation or not
  //    }
  //  }

  def getCurrentConditions(not_factor : NotFactor) : CNF = {
    println(" - current conditions - not factor")
    var clauses : Set[Clause] = Set()
    if (not_factor.t) { // 'not' present
      not_factor.factor match {
        case Expression(terms) =>
          if (terms.length > 1) {
            for (term <- terms) {
              clauses = clauses ++ getCurrentConditions(negate(term)).clauses
            }
          }
          else { // cannot be empty
            val term = terms.head
            if(term.not_factors.length > 1) {
              for (not_factor <- term.not_factors) {
                clauses = mergeCNF(CNF(clauses), getCurrentConditions(negate(not_factor))).clauses
              }
            }
            else { // cannot be empty
              clauses = clauses ++ getCurrentConditions(negate(term.not_factors.head)).clauses
            }
          }
        case Variable(name) =>
          println(" - variable - " ++ name)
          val literal = Literal(name, negation = true)
          val clause = Clause(Set(literal))
          clauses = clauses ++ Set(clause)
      }
      println("1")
      println(clauses)
      CNF(clauses)
    }
    else { // 'not' not present
      not_factor.factor match {
        case Expression(terms) =>
          println("1.5")
          getCurrentConditions(Expression(terms))
        case Variable(name) =>
          println(" - variable - " ++ name)
          val literal = Literal(name, negation = false)
          val clause = Clause(Set(literal))
          println("2")
          println(clause)
          CNF(Set(clause)) // base case
        //Set[Set[(String, Boolean)]]() + ((name, true)) // base case, change to if negation or not
      }
    }
  }

  def getCurrentConditions(term : Term) : CNF = {
    println(" - current conditions - term")
    val not_factors = term.not_factors
    var clauses : Set[Clause] = Set()
    for (not_factor <- not_factors) {
      clauses = clauses ++ getCurrentConditions(not_factor).clauses
    }
    println("3")
    println(clauses)
    CNF(clauses)
  }

  def getCurrentConditions(expression : Expression) : CNF = {
    println(" - current conditions - expression")
    val terms = expression.terms
    var clauses : Set[Clause] = Set()
    for (term <- terms) {
      clauses = mergeCNF(CNF(clauses), getCurrentConditions(term)).clauses
    }
    println("4")
    println(clauses)
    CNF(clauses)
  }

  def cnfToString(cnf : CNF): Unit = {
    println("~~~ CNF ~~~")
    for (clause <- cnf.clauses) {
      print("(")
      for (literal <- clause.literals) {
        print("(")
        if (literal.negation) {
          print(" NOT")
        }
        print(" " ++ literal.variable ++ ") OR")
      }
      println(") AND")
    }
    println("~~~ ~~~ ~~~")

  }

  def rebuilding(statement: Statement) : String = {
    statement match {
      case ReceiveStatement(label, types, condition, continuation) =>
        var receiveStatement = "?" + label + "("
        for (typ <- types) {
          receiveStatement = receiveStatement ++ typ._1 ++ " : " ++ typ._2 ++ ", "
        }
        if (types.nonEmpty) receiveStatement = receiveStatement.dropRight(2)
        receiveStatement = receiveStatement + ")"
        if (condition != null) { // condition.terms.nonEmpty
          receiveStatement = receiveStatement ++ "["
          receiveStatement = receiveStatement ++ condition // expressionPT(condition)
          receiveStatement = receiveStatement ++ "]"
        }

        if (continuation != null) {
          val continuationTemp = rebuilding(continuation)
          if (continuationTemp != "") {
            receiveStatement = receiveStatement ++ "."
            receiveStatement = receiveStatement ++ continuationTemp
          }
        }

        receiveStatement

      case ReceiveChoiceStatement(label, choices) =>
        var receiveChoiceStatement = ""
        if (choices.length == 1) {
          receiveChoiceStatement = receiveChoiceStatement + rebuilding(choices.head)
          receiveChoiceStatement
        }
        else {
          receiveChoiceStatement = "&{"
          for (choice <- choices) {
            receiveChoiceStatement = receiveChoiceStatement + rebuilding(choice) + ", "
          }
          if (choices.nonEmpty) receiveChoiceStatement = receiveChoiceStatement.dropRight(2)
          receiveChoiceStatement = receiveChoiceStatement + "}"
          receiveChoiceStatement
        }

      case SendStatement(label, types, condition, continuation) =>
        var sendStatement = "!" + label + "("
        for (typ <- types) {
          sendStatement = sendStatement ++ typ._1 ++ " : " ++ typ._2 ++ ", "
        }
        if (types.nonEmpty) sendStatement = sendStatement.dropRight(2)
        sendStatement = sendStatement + ")"
        if (condition != null) { // condition.terms.nonEmpty
          sendStatement = sendStatement ++ "["
          sendStatement = sendStatement ++ condition // expressionPT(condition)
          sendStatement = sendStatement ++ "]"
        }

        if (continuation != null) {
          val continuationTemp = rebuilding(continuation)
          if (continuationTemp != "") {
            sendStatement = sendStatement ++ "."
            sendStatement = sendStatement ++ continuationTemp
          }
        }

        sendStatement

      case SendChoiceStatement(label, choices) =>
        var sendChoiceStatement = ""
        if (choices.length == 1) {
          sendChoiceStatement = sendChoiceStatement + rebuilding(choices.head)
          sendChoiceStatement
        }
        else {
          sendChoiceStatement = "+{"
          for (choice <- choices) {
            sendChoiceStatement = sendChoiceStatement + rebuilding(choice) + ", "
          }
          if (choices.nonEmpty) sendChoiceStatement = sendChoiceStatement.dropRight(2)
          sendChoiceStatement = sendChoiceStatement + "}"
          sendChoiceStatement
        }

      case RecursiveStatement(label, body) =>
        if (body != null)
          "rec " + label + ".(" + rebuilding(body) + ")"
        else
          ""

      case RecursiveVar(name, continuation) =>
        var recursiveVar = name
        if (continuation != null) {
          recursiveVar = recursiveVar + "." + rebuilding(continuation)
        }
        recursiveVar

      case End() =>
        ""

      case null =>
        ""

      case _ =>
        throw new Exception("Error: Statement does not match")
    }
  }

  def rebuildST(sessionType: SessionType): Unit = {
    val rebuiltST = sessionType.name + " = " + rebuilding(sessionType.statement)
    val pw = new PrintWriter(new File("examples/src/main/scala/monitor/examples/solvedST/" + sessionType.name + ".st"))
    pw.write(rebuiltST)
    pw.close()
  }

}
