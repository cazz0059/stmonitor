package monitor.parser

import monitor.model._
//import java.io.*
import com.typesafe.scalalogging.Logger

//import scala.collection.mutable
//import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.xml.{PrettyPrinter, XML}

class STParseTree(sessionType: SessionType, name: String) {

  val logger = Logger("STParseTree")
  val helper = new STSolverHelper()

  def construct() : Unit = {
    var textXML = ""

    val STname = sessionType.name
    logger.info("Starting construction")
    textXML = textXML ++ "<" ++ STname ++ ">"
    textXML = textXML ++ statementPT(sessionType.statement)
    textXML = textXML ++ "</" ++ STname ++ ">"

    print("\n" ++ textXML ++ "\n\n")//

    val xmlText = XML.loadString(textXML)
//    val parseTree = new PrettyPrinter(150, 4).format(xmlText)
//    print("\n")//
//    print(parseTree)//
//    print("\n\n")//
    XML.save("examples/src/main/scala/monitor/examples/parseTrees/" ++ STname ++ "_" ++ name ++ "_parseTree.xml", xmlText)
    logger.info("XML file created")
    println()
    //logger.info("XML text created")//
  }

  def statementPT(root : Statement) : String = {
    root match {
      case ReceiveStatement(label, id, types, condition, continuation) =>
        //logger.info("ReceiveStatement" ++ label)//
        var textXML = "<receive_" ++ label ++ ">"

        textXML = textXML ++ "<types>"
        textXML = textXML ++ typesPT(types)
        textXML = textXML ++ "</types>"

        if (condition != null) { // condition.terms.nonEmpty
          textXML = textXML ++ "<conditions "
          textXML = textXML ++ "assertions=\"" ++ conditionPT(condition) ++ "\"/>" // expressionPT(condition)
          //textXML = textXML ++ "/>"
        }

        if (continuation != null) {
          textXML = textXML ++ "<continuation>"
          textXML = textXML ++ statementPT(continuation)
          textXML = textXML ++ "</continuation>"
        }

        textXML ++ "</receive_" ++ label ++ ">"

      case SendStatement(label, id, types, condition, continuation) =>
        //logger.info("SendStatement " ++ label)//
        var textXML = "<send_" ++ label ++ ">"

        textXML = textXML ++ "<types>"
        textXML = textXML ++ typesPT(types)
        textXML = textXML ++ "</types>"

        if(condition != null) {
          textXML = textXML ++ "<conditions "
          textXML = textXML ++ "assertions=\"" ++ conditionPT(condition) ++ "\"/>" // expressionPT(condition)
          //textXML = textXML ++ "/>"
        }

        if (continuation != null) {
          textXML = textXML ++ "<continuation>"
          textXML = textXML ++ statementPT(continuation)
          textXML = textXML ++ "</continuation>"
        }

        textXML ++ "</send_" ++ label ++ ">"

      case ReceiveChoiceStatement(label, choices) =>
        //logger.info("ReceiveChoiceStatement")//
        var textXML = "<branch_" ++ label ++ ">"

        for (choice <- choices) {
          textXML = textXML ++ "<choice>"
          textXML = textXML ++ statementPT(choice)
          textXML = textXML ++ "</choice>"
        }

        textXML ++ "</branch_" ++ label ++ ">"

      case SendChoiceStatement(label, choices) =>
        //logger.info("SendChoiceStatement")//
        var textXML = "<selection_" ++ label ++ ">"

        for (choice <- choices) {
          textXML = textXML ++ "<choice>"
          textXML = textXML ++ statementPT(choice)
          textXML = textXML ++ "</choice>"
        }

        textXML ++ "</selection_" ++ label ++ ">"

      case RecursiveStatement(label, body) =>
        //logger.info("Recursive Statement")//
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
        //logger.info("RecursiveVarStatement")//
        var textXML = "<recursionVar_" ++ name ++ ">"
        textXML = textXML ++ statementPT(continuation)
        textXML ++ "</recursionVar_" ++ name ++ ">"

      case End() =>
        //logger.info("End")//
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
    //conditions
  }

//  def expressionPT(expression: Expression) : String = {
//    expression match {
//      case Expression(terms) =>
//        logger.info("Conditions")
//        var textXML = ""
//        if (terms.length > 1) {
//          for (term <- terms) {
//            textXML = textXML ++ "<or>"
//            textXML = textXML ++ termPT(term)
//            textXML = textXML ++ "</or>"
//          }
//          textXML
//        }
//        else {
//          textXML ++ termPT(terms.head)
//        }
//    }
////    var textXML = ""
////    var condition_temp = condition
////    condition_temp = condition_temp.replace("<", "&lt;")
////    condition_temp = condition_temp.replace(">", "&gt;")
////    // add () && () //
////    // add or //
////    // add !something //
////    // add !(something) //
////    // add !=
////    // add parse for <, >, <= and >= - these first to check game.st //
////    // recursion on condition
////
////    // this is wrong because of nested logical operations, cannot tell the parsing order
////    // parsing the conditions should be part of the parser
////    if (condition_temp.take(1) == "!"){
////      textXML = textXML ++ "<not>"
////      var con_temp = condition_temp
////      con_temp = con_temp.trim
////      if (con_temp.take(1) == "(" && con_temp.takeRight(1) == ")"){
////        con_temp = con_temp.drop(1).dropRight(1)
////      }
////      textXML = textXML ++ "<condition>"
////      textXML = textXML ++ con_temp
////      textXML = textXML ++ "</condition>"
////
////      textXML = textXML ++ "</not>"
////    }
////
////    else {
////      textXML = textXML ++ "<condition>"
////      textXML = textXML ++ condition_temp
////      textXML = textXML ++ "</condition>"
////    }
////    textXML
//  }
//
//  def termPT(term: Term) : String = {
//    term match {
//      case Term(not_factors) =>
//        //logger.info("Terms")
//        var textXML = ""
//        if (not_factors.length > 1) {
//          for (not_factor <- not_factors) {
//            textXML = textXML ++ "<and>"
//            textXML = textXML ++ not_factorsPT(not_factor)
//            textXML = textXML ++ "</and>"
//          }
//          textXML
//        }
//        else {
//          textXML ++ not_factorsPT(not_factors.head)
//        }
//    }
//  }
//
//  def not_factorsPT(not_factor: NotFactor) : String = {
//    logger.info("Current Not Factor" ++ helper.conditionToString(not_factor))
//    not_factor match {
//      case NotFactor(t, factor) =>
//        var textXML = ""
//        if (t) {
//          textXML = textXML ++ "<not>"
//          textXML = textXML ++ factorPT(factor)
//          textXML ++ "</not>"
//        }
//        else {
//          textXML ++ factorPT(factor)
//        }
//    }
//  }
//
//  def factorPT(factor: Factor) : String = {
//    factor match {
//      case Expression(terms) =>
//        expressionPT(Expression(terms))
//      case Variable(name) =>
//        var name_temp = name
//        name_temp = name_temp.replace("<", "&lt;")
//        name_temp = name_temp.replace(">", "&gt;")
//        name_temp
//    }
//  } // see where arrange the < and > in string
//


//  def ineq(condition : String) : String = {
//    var textXML = ""
//
//  }

  //        if (conditions(i).contains("<") || conditions(i).contains(">")){
  //          textXML = textXML ++ ineq(conditions(i))
  //        }
  //        else {
  //          textXML = textXML ++ conditions(i)
  //        }

  //////////////////////
  //    else if (condition_temp.contains(" && ") || condition_temp.contains("&&")){
  //      textXML = textXML ++ "<and>"
  //      val conditions = condition_temp.split("&&")
  //      for (i <- 0 until conditions.length) {
  //        var con_temp = conditions(i)
  //        con_temp = con_temp.trim
  //        if (con_temp.take(1) == "(" && con_temp.takeRight(1) == ")"){
  //          con_temp = con_temp.drop(1).dropRight(1)
  //        }
  //        textXML = textXML ++ "<condition>"
  //        textXML = textXML ++ con_temp
  //        textXML = textXML ++ "</condition>"
  //      }
  //      textXML = textXML ++ "</and>"
  //    }
  //    else if (condition_temp.contains(" || ") || condition_temp.contains("||")){
  //      textXML = textXML ++ "<or>"
  //      val conditions = condition_temp.split("||")
  //      for (i <- 0 until conditions.length) {
  //        var con_temp = conditions(i)
  //        con_temp = con_temp.trim
  //        if (con_temp.take(1) == "(" && con_temp.takeRight(1) == ")"){
  //          con_temp = con_temp.drop(1).dropRight(1)
  //        }
  //        textXML = textXML ++ "<condition>"
  //        textXML = textXML ++ con_temp
  //        textXML = textXML ++ "</condition>"
  //      }
  //      textXML = textXML ++ "</or>"
  //    }
  ///////////////////////

}
