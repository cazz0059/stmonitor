package monitor.parser

import scala.meta._
import monitor.model._
import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class STSolverHelper {

  var fullTraces : mutable.LinkedHashMap[ListBuffer[String], Boolean] = mutable.LinkedHashMap()

  def addToTraces(currTrace : ListBuffer[String], verdict : Boolean) : Unit = {
    fullTraces += (currTrace -> verdict)
//    println("Added to fulltraces")
//    println(currTrace)
  }

  def getCurrentTrace(statement: Statement, currentTrace : ListBuffer[String]) : Unit = {

    statement match {

      case ReceiveStatement(label, id, types, condition, continuation) =>
        val labelList : ListBuffer[String] = ListBuffer(label)
        val currTrace : ListBuffer[String] = currentTrace ++ labelList
        continuation match {
          case End() =>
            addToTraces(currTrace, true)
          case null =>
            addToTraces(currTrace, true)
          case _ =>
            getCurrentTrace(continuation, currTrace)
        }

      case ReceiveChoiceStatement(label, choices) =>
        for (choice <- choices) {
          getCurrentTrace(choice, currentTrace)
        }

      case SendStatement(label, id, types, condition, continuation) =>
        val labelList : ListBuffer[String] = ListBuffer(label)
        val currTrace : ListBuffer[String] = currentTrace ++ labelList
        continuation match {
          case End() =>
            addToTraces(currTrace, true)
          case null =>
            addToTraces(currTrace, true)
          case _ =>
            getCurrentTrace(continuation, currTrace)
        }

      case SendChoiceStatement(label, choices) =>
        for (choice <- choices) {
          getCurrentTrace(choice, currentTrace)
        }

      case RecursiveStatement(label, body) =>
        getCurrentTrace(body, currentTrace)

      case RecursiveVar(name, continuation) =>
        getCurrentTrace(continuation, currentTrace)

      case End() =>
        addToTraces(currentTrace, true)

      case null =>
        addToTraces(currentTrace, true)
    }
  }

  def updateTraces(traceLabels : List[String], allTraces : mutable.LinkedHashMap[ListBuffer[String], Boolean]) : mutable.LinkedHashMap[ListBuffer[String], Boolean] = {
    var traces = allTraces
    val fullTraceString = traceLabels.mkString(" ")
    //println("Using " + fullTraceString)
    for (trc <- allTraces) {
      // Reversing the order
      val trcReversed = trc._1.toList.reverse
      val trcString = trcReversed.mkString(" ")
      //println("Comparing with " + trcString)

      if (traceLabels.forall(trc._1.toList.contains)) {
        //println("Matched")
        traces.put(trc._1, false)
      }
      else {
        //println("Unmatched")
      }
    }

    traces
  }

  def getAllTraces(sessionType : SessionType) : mutable.LinkedHashMap[ListBuffer[String], Boolean] = {
    getCurrentTrace(sessionType.statement, ListBuffer())
    fullTraces
  }

  def printTrace(trace : ListBuffer[String]) : String = {
    var traceString = ""
    for(event <- trace) {
      traceString = traceString + " > " + event
    }
    traceString
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
        if (condition != null) {
          receiveStatement = receiveStatement ++ "["
          receiveStatement = receiveStatement ++ condition
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
        if (condition != null) {
          sendStatement = sendStatement ++ "["
          sendStatement = sendStatement ++ condition
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
        if ((continuation != null) && (continuation != End())) {
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
    val pw = new PrintWriter(new File("examples/src/main/scala/examples/optimisedST/" + sessionType.name + ".st"))
    pw.write(rebuiltST)
    pw.close()
  }

}
