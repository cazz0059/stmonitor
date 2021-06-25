package monitor.parser

import monitor.model._
import com.typesafe.scalalogging.Logger

import scala.xml.{PrettyPrinter, XML}

class STParseTree(sessionType: SessionType, name: String) {

  val helper = new STSolverHelper()

  def construct() : Unit = {
    var textXML = ""

    val STname = sessionType.name
    textXML = textXML ++ "<" ++ STname ++ ">"
    textXML = textXML ++ statementPT(sessionType.statement)
    textXML = textXML ++ "</" ++ STname ++ ">"

    val xmlText = XML.loadString(textXML)
    XML.save("examples/src/main/scala/examples/parseTrees/" ++ STname ++ "_" ++ name ++ "_parseTree.xml", xmlText)
  }

  def statementPT(root : Statement) : String = {
    root match {
      case ReceiveStatement(label, id, types, condition, continuation) =>
        var textXML = "<receive_" ++ label ++ ">"

        textXML = textXML ++ "<types>"
        textXML = textXML ++ typesPT(types)
        textXML = textXML ++ "</types>"

        if (condition != null) {
          textXML = textXML ++ "<conditions "
          textXML = textXML ++ "assertions=\"" ++ conditionPT(condition) ++ "\"/>"
        }

        if (continuation != null) {
          textXML = textXML ++ "<continuation>"
          textXML = textXML ++ statementPT(continuation)
          textXML = textXML ++ "</continuation>"
        }

        textXML ++ "</receive_" ++ label ++ ">"

      case SendStatement(label, id, types, condition, continuation) =>
        var textXML = "<send_" ++ label ++ ">"

        textXML = textXML ++ "<types>"
        textXML = textXML ++ typesPT(types)
        textXML = textXML ++ "</types>"

        if(condition != null) {
          textXML = textXML ++ "<conditions "
          textXML = textXML ++ "assertions=\"" ++ conditionPT(condition) ++ "\"/>"
        }

        if (continuation != null) {
          textXML = textXML ++ "<continuation>"
          textXML = textXML ++ statementPT(continuation)
          textXML = textXML ++ "</continuation>"
        }

        textXML ++ "</send_" ++ label ++ ">"

      case ReceiveChoiceStatement(label, choices) =>
        var textXML = "<branch_" ++ label ++ ">"

        for (choice <- choices) {
          textXML = textXML ++ "<choice>"
          textXML = textXML ++ statementPT(choice)
          textXML = textXML ++ "</choice>"
        }

        textXML ++ "</branch_" ++ label ++ ">"

      case SendChoiceStatement(label, choices) =>
        var textXML = "<selection_" ++ label ++ ">"

        for (choice <- choices) {
          textXML = textXML ++ "<choice>"
          textXML = textXML ++ statementPT(choice)
          textXML = textXML ++ "</choice>"
        }

        textXML ++ "</selection_" ++ label ++ ">"

      case RecursiveStatement(label, body) =>
        var textXML = ""
        if (body != null) {
          textXML = "<recursion_" ++ label ++ ">"
          textXML = textXML ++ statementPT(body)
          textXML ++ "</recursion_" ++ label ++ ">"
        }
        else {
          textXML
        }

      case RecursiveVar(name, continuation) =>
        var textXML = "<recursionVar_" ++ name ++ ">"
        textXML = textXML ++ statementPT(continuation)
        textXML ++ "</recursionVar_" ++ name ++ ">"

      case End() =>
        ""

      case null =>
        ""

      case _ =>
        throw new Exception("Error: Statement does not match")
    }

  }

  def typesPT(types : Map[String, String]) : String = {
    var textXML = ""
    for (typ <- types){
      textXML = textXML ++ typ._1 ++ " : " ++ typ._2 ++ ", "
    }
    textXML = textXML.dropRight(2)
    textXML
  }

  def conditionPT(conditions : String) : String = {
    var condition_temp = conditions
    condition_temp = condition_temp.replace("&", "&amp;")
    condition_temp = condition_temp.replace("<", "&lt;")
    condition_temp = condition_temp.replace(">", "&gt;")
    condition_temp = condition_temp.replace("|", "&#124;")
    condition_temp = condition_temp.replace("=", "&#61;")
    condition_temp
  }

}
