package monitor.parser

import java.sql.Statement

import scala.collection.mutable
import scala.reflect.runtime.currentMirror


class STSolver(sessionType : SessionType){

  val logger = Logger("STSolver")

  private val toolbox = currentMirror.mkToolBox()

  private var scopes = new mutable.HashMap[String, Scope]()
  private var curScope = "global"
  scopes(curScope) = new Scope("global", null)

  private var branches = new mutable.HashMap[Statement, String]()

  def getRecursiveVarScope(recursiveVar: RecursiveVar): Scope = {
    checkRecVariable(scopes(curScope), recursiveVar)
  }

  def getCurScope(): String ={
    curScope
  }

  def getScope(scopeName: String): Scope = {
    scopes(scopeName)
  }

  def run() : Unit = {
    sessionType.statement match {
      case recursiveStatement: RecursiveStatement =>
        var tmpStatement: Statement = null
        while(tmpStatement.isInstanceOf[RecursiveStatement]){
          tmpStatement = recursiveStatement.body
        }
        checkStatement(recursiveStatement.body) // check if this is the right method, becaus eininterpreter it is that monitor is initialised, not the whole statement checked
      case _ =>
        checkStatement(sessionType.statement) // same here
    }

    initialWalk(sessionType.statement)
    curScope = "global"

    walk(sessionType.statement)
  }

  def initialWalk(root: Statement): Unit = {
    root match {
      case ReceiveStatement(label, types, condition, continuation) =>
        createAndUpdateScope(label)
        checkAndInitVariables(label, types, condition)
        synthMon.handlePayloads(label, types)
        initialWalk(continuation)

      case SendStatement(label, types, condition, continuation) =>
        createAndUpdateScope(label)
        checkAndInitVariables(label, types, condition)
        synthMon.handlePayloads(label, types)
        initialWalk(continuation)

      case ReceiveChoiceStatement(label, choices) =>
        createAndUpdateScope(label)
        for(choice <- choices) {
          createAndUpdateScope(choice.asInstanceOf[ReceiveStatement].label)
          checkAndInitVariables(choice.asInstanceOf[ReceiveStatement].label, choice.asInstanceOf[ReceiveStatement].types, choice.asInstanceOf[ReceiveStatement].condition)
          synthMon.handlePayloads(choice.asInstanceOf[ReceiveStatement].label, choice.asInstanceOf[ReceiveStatement].types)
          initialWalk(choice.asInstanceOf[ReceiveStatement].continuation)
          curScope = scopes(choice.asInstanceOf[ReceiveStatement].label).parentScope.name
        }

      case SendChoiceStatement(label, choices) =>
        createAndUpdateScope(label)
        for(choice <- choices) {
          createAndUpdateScope(choice.asInstanceOf[SendStatement].label)
          checkAndInitVariables(choice.asInstanceOf[SendStatement].label, choice.asInstanceOf[SendStatement].types, choice.asInstanceOf[SendStatement].condition)
          synthMon.handlePayloads(choice.asInstanceOf[SendStatement].label, choice.asInstanceOf[SendStatement].types)
          initialWalk(choice.asInstanceOf[SendStatement].continuation)
          curScope = scopes(choice.asInstanceOf[SendStatement].label).parentScope.name
        }

      case RecursiveStatement(label, body) =>
        scopes(curScope).recVariables(label) = body
        initialWalk(body)

      case RecursiveVar(name, continuation) =>
        initialWalk(continuation)

      case End() =>
    }
  }

  private def createAndUpdateScope(label: String): Unit = {
    scopes(label) = new Scope(label, scopes(curScope))
    curScope = label
  }

  private def checkAndInitVariables(label: String, types: Map[String, String], condition: String): Unit = {
    for(typ <- types) {
      scopes(curScope).variables(typ._1) = (false, typ._2)
    }
    if (condition != null){
      val identifiersInCondition = getIdentifiers(condition)
      for(ident <- identifiersInCondition){
        val identScope = searchIdent(curScope, ident)
        if(identScope != curScope) {
          scopes(identScope).variables(ident) = (true, scopes(identScope).variables(ident)._2)
        }
      }
    }
  }

  class traverser extends Traverser {
    var identifiers: List[String] = List[String]()

    override def traverse(tree: Tree): Unit = tree match { // change this so it doesnt only return identifiers?
      case i @ Ident(_) =>
        identifiers = i.name.decodedName.toString :: identifiers
        super.traverse(tree)
      case _ =>
        super.traverse(tree)
    }
  }

  def getIdentifiers(condition: String): List[String] = { // check this tree
    val conditionTree = toolbox.parse(condition)
    val traverser = new traverser
    traverser.traverse(conditionTree)
    traverser.identifiers.distinct.filter(_ != "util")
  }

  def checkStatement(statement: Statement) : Unit = {

  }


}