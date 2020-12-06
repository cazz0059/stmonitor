package monitor.parser

import monitor.model._
//import java.io.*
import com.typesafe.scalalogging.Logger

//import scala.collection.mutable
//import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.xml.{PrettyPrinter, XML}

class STParseTree(sessionType: SessionType, name: String) {

  val logger = Logger("STParseTree")

  def construct() : Unit = {
    var textXML = ""

    var STname = sessionType.name
    logger.info("Starting construction")
    textXML = textXML ++ "<" ++ STname ++ ">"
    textXML = textXML ++ statementPT(sessionType.statement)
    textXML = textXML ++ "</" ++ STname ++ ">"

    //print("\n" ++ textXML ++ "\n\n")

    val xmlText = XML.loadString(textXML)
    val parseTree = new PrettyPrinter(150, 4).format(xmlText)
    print("\n")
    print(parseTree)
    print("\n\n")
    XML.save(name ++ "_" ++ STname ++ "_parseTree.xml", xmlText)
    logger.info("XML file created")
    //logger.info("XML text created")
  }

  def statementPT(root : Statement) : String = {
    root match {
      case ReceiveStatement(label, types, condition, continuation) =>
        //logger.info("ReceiveStatement")
        var textXML = "<receive_" ++ label ++ ">"

        textXML = textXML ++ "<types>"
        textXML = textXML ++ typesPT(types)
        textXML = textXML ++ "</types>"

        if (condition != null) {
          textXML = textXML ++ "<condition>"
          textXML = textXML ++ conditionPT(condition)
          textXML = textXML ++ "</condition>"
        }

        textXML = textXML ++ "<continuation>"
        textXML = textXML ++ statementPT(continuation)
        textXML = textXML ++ "</continuation>"

        textXML ++ "</receive_" ++ label ++ ">"

      case SendStatement(label, types, condition, continuation) =>
        //logger.info("SendStatement " ++ label)
        var textXML = "<send_" ++ label ++ ">"

        textXML = textXML ++ "<types>"
        textXML = textXML ++ typesPT(types)
        textXML = textXML ++ "</types>"

        if(condition != null) {
          textXML = textXML ++ "<condition>"
          textXML = textXML ++ conditionPT(condition)
          textXML = textXML ++ "</condition>"
        }

        textXML = textXML ++ "<continuation>"
        textXML = textXML ++ statementPT(continuation)
        textXML = textXML ++ "</continuation>"

        textXML ++ "</send_" ++ label ++ ">"

      case ReceiveChoiceStatement(label, choices) =>
        //logger.info("ReceiveChoiceStatement")
        var textXML = "<branch_" ++ label ++ ">"

        for (choice <- choices) {
          textXML = textXML ++ "<choice>"
          textXML = textXML ++ statementPT(choice)
          textXML = textXML ++ "</choice>"
        }

        textXML ++ "</branch_" ++ label ++ ">"

      case SendChoiceStatement(label, choices) =>
        //logger.info("SendChoiceStatement")
        var textXML = "<selection_" ++ label ++ ">"

        for (choice <- choices) {
          textXML = textXML ++ "<choice>"
          textXML = textXML ++ statementPT(choice)
          textXML = textXML ++ "</choice>"
        }

        textXML ++ "</selection_" ++ label ++ ">"

      case RecursiveStatement(label, body) =>
        //logger.info("Recursivetatement")
        var textXML = "<recursion_" ++ label ++ ">"
        textXML = textXML ++ statementPT(body)
        textXML ++ "</recursion_" ++ label ++ ">"

      case RecursiveVar(name, continuation) =>
        //logger.info("RecursiveVarStatement")
        var textXML = "<recursionVar_" ++ name ++ ">"
        textXML = textXML ++ statementPT(continuation)
        textXML ++ "</recursionVar_" ++ name ++ ">"

      case End() =>
        //logger.info("End")
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
    textXML
  }

  def conditionPT(condition : String) : String = {
    condition
  }


}
