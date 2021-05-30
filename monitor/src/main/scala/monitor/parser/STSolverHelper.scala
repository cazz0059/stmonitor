package monitor.parser

//import scala.collection.mutable
//import scala.reflect.runtime._
//import scala.reflect.runtime.universe._
//import scala.tools.reflect.ToolBox

import scala.meta._
import monitor.model._
import java.io.{File, PrintWriter}
//import monitor.model.Scope
import scala.collection.mutable.ListBuffer

class STSolverHelper {

  var fullTraces : Map[ListBuffer[String], Boolean] = Map()

  def getCurrentTrace(statement: Statement, currentTrace : ListBuffer[String]) : Unit = { // ListBuffer[String]
    var currTrace = currentTrace
    statement match {
      case ReceiveStatement(label, id, types, condition, continuation) =>
        currTrace += label
        getCurrentTrace(continuation, currTrace)
      case ReceiveChoiceStatement(label, choices) =>
        for (choice <- choices)
          getCurrentTrace(choice, currTrace)
      case SendStatement(label, id, types, condition, continuation) =>
        currTrace += label
        getCurrentTrace(continuation, currTrace)
      case SendChoiceStatement(label, choices) =>
        for (choice <- choices)
          getCurrentTrace(choice, currTrace)
      case RecursiveStatement(label, body) =>
        getCurrentTrace(body, currTrace)
      case RecursiveVar(name, continuation) =>
        getCurrentTrace(continuation, currTrace)
      case End() =>
        // add trace to the map
        fullTraces = fullTraces + (currTrace -> true)
    }
  }

  def getAllTraces(sessionType : SessionType) : Map[ListBuffer[String], Boolean] = {
    getCurrentTrace(sessionType.statement, ListBuffer())
    fullTraces
  }

  def aggCondsToString(aggConds : List[String]) : String = {
    var aggCondsString = aggConds.head
    val tempAggConds = aggConds.tail
    for (cond <- tempAggConds) {
      aggCondsString = "(" + aggCondsString + ") && (" + cond + ")"
    }
    aggCondsString
  }

  def aggCondsToTree(aggConds : List[String]) : Set[scala.meta.Term] = {
    var setOfConds : Set[scala.meta.Term] = Set()
    for(cond <- aggConds) {
      val condTree : scala.meta.Term = cond.parse[scala.meta.Term].get
      setOfConds = setOfConds ++ Set(condTree)
    }
    setOfConds
  }

  def rebuilding(statement: Statement) : String = {
    statement match {
      case ReceiveStatement(label, id, types, condition, continuation) =>
        var receiveStatement = "!" + label + "("
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
          receiveChoiceStatement = "+{"
          for (choice <- choices) {
            receiveChoiceStatement = receiveChoiceStatement + rebuilding(choice) + ", "
          }
          if (choices.nonEmpty) receiveChoiceStatement = receiveChoiceStatement.dropRight(2)
          receiveChoiceStatement = receiveChoiceStatement + "}"
          receiveChoiceStatement
        }

      case SendStatement(label, id, types, condition, continuation) =>
        var sendStatement = "?" + label + "("
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
          sendChoiceStatement = "&{"
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
    val pw = new PrintWriter(new File("examples/src/main/scala/examples/solvedST/" + sessionType.name + ".st"))
    pw.write(rebuiltST)
    pw.close()
  }

}
