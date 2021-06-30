package monitor.parser

import monitor.model._
import monitor.model.Scope

import scala.collection.mutable
import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

import com.microsoft.z3._

import java.io._

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._


class STSolver(sessionType : SessionType, path: String, preamble: String){

  private val toolbox = currentMirror.mkToolBox()

  private val helper = new STSolverHelper()
  private val solver = new STSolverZ3

  private var scopes = new mutable.HashMap[String, Scope]()
  private var curScope = "global"
  scopes(curScope) = new Scope(curScope, curScope, null)

  private var solvedST : SessionType = new SessionType(null, null)

  private var allTraces : mutable.LinkedHashMap[ListBuffer[String], Boolean] = helper.getAllTraces(sessionType)

  def getRecursiveVarScope(recursiveVar: RecursiveVar): Scope = {
    checkRecVariable(scopes(curScope), recursiveVar)
  }

  def getCurScope: String ={
    curScope
  }

  def getScope(scopeName: String): Scope = {
    scopes(scopeName)
  }

  private var events : List[(String, BoolExpr)] = List()
  def addEvent(label : String, boolExpr : BoolExpr) : Unit = {
    events = (label, boolExpr) :: events
  }
  def getEvent(label : String) : BoolExpr = {
    val event = events.filter(ev=>ev._1 == label)
    if(event.size == 1)
      return event.head._2
    else
      println("ERROR: labelled event not found"); null
  }


  def run() : SessionType = {

    initialWalk(sessionType.statement)
    curScope = "global"

    // getting util file contents
    val source = scala.io.Source.fromFile(path + "/util.scala", "utf-8")
    val util = try source.mkString finally source.close()
    generateUtilFunctionModels(util)

    solvedST.name = sessionType.name
    solvedST.statement = walk(sessionType.statement)
    if(solvedST.statement == null)
      solvedST.statement = End()

    val pw = new PrintWriter(new File("examples/src/main/scala/examples/traceStats/" ++ sessionType.name ++ "_unreachability_status_table.txt" ))
    var table = "Reachable? |  Trace  \n"
    for(trace <- allTraces){
      if(trace._2){
        table = table + "Yes        |  " + helper.printTrace(trace._1) + "\n"
      }
      else {
        table = table + "No         |  " + helper.printTrace(trace._1) + "\n"
      }
    }
    pw.write(table)
    pw.close

    solvedST
  }

  def initialWalk(root: Statement): Unit = {
    root match {
      case ReceiveStatement(label, id, types, condition, continuation) =>
        createAndUpdateScope(label) // creates new scope
        checkAndInitVariables(label, types, condition) // puts the variables in the scopes
        initialWalk(continuation)

      case SendStatement(label, id, types, condition, continuation) =>
        createAndUpdateScope(label)
        checkAndInitVariables(label, types, condition)
        initialWalk(continuation)

      case ReceiveChoiceStatement(label, choices) =>
        createAndUpdateScope(label)
        for(choice <- choices) {
          createAndUpdateScope(choice.asInstanceOf[ReceiveStatement].label)
          checkAndInitVariables(choice.asInstanceOf[ReceiveStatement].label, choice.asInstanceOf[ReceiveStatement].types, choice.asInstanceOf[ReceiveStatement].condition)
          initialWalk(choice.asInstanceOf[ReceiveStatement].continuation)
          curScope = scopes(choice.asInstanceOf[ReceiveStatement].label).parentScope.name
        }

      case SendChoiceStatement(label, choices) =>
        createAndUpdateScope(label)
        for(choice <- choices) {
          createAndUpdateScope(choice.asInstanceOf[SendStatement].label)
          checkAndInitVariables(choice.asInstanceOf[SendStatement].label, choice.asInstanceOf[SendStatement].types, choice.asInstanceOf[SendStatement].condition)
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

  def walk(statement: Statement): Statement = {
    statement match {
      case statement @ ReceiveStatement(label, id, types, condition, _) =>
        curScope = label
        checkCondition(label, types, condition)
        val trace = scopes(curScope).trace

        if(condition == null || solver(trace)) {
          ReceiveStatement(label, id, types, condition, walk(statement.continuation))
        }
        else {
          null
        }

      case statement @ SendStatement(label, id, types, condition, _) =>
        curScope = label
        checkCondition(label, types, condition)
        val trace = scopes(curScope).trace
        if(condition == null || solver(trace)) {
          SendStatement(label, id, types, condition, walk(statement.continuation))
        }
        else {
          null
        }

      case statement @ ReceiveChoiceStatement(label, choices) =>
        curScope = label
        var solvedChoices : ListBuffer[Statement] = ListBuffer()

        for(choice <- choices) {
          val currChoice = choice.asInstanceOf[ReceiveStatement]
          curScope = currChoice.label
          checkCondition(currChoice.label, currChoice.types, currChoice.condition)
          val trace = scopes(curScope).trace

          if (currChoice.condition == null || solver(trace)) {
            solvedChoices += ReceiveStatement(currChoice.label, currChoice.statementID, currChoice.types, currChoice.condition, walk(currChoice.continuation))
            curScope = scopes(currChoice.label).parentScope.name
          }

        }

        if (solvedChoices.nonEmpty) {
          ReceiveChoiceStatement(label, solvedChoices.toList)
        }
        else {
          null
        }

      case statement @ SendChoiceStatement(label, choices) =>
        curScope = label
        var solvedChoices : ListBuffer[Statement] = ListBuffer()

        for(choice <- choices) {
          val currChoice = choice.asInstanceOf[SendStatement]
          curScope = currChoice.label
          checkCondition(currChoice.label, currChoice.types, currChoice.condition)
          val trace = scopes(curScope).trace

          if (currChoice.condition == null || solver(trace)) {
            solvedChoices += SendStatement(currChoice.label, currChoice.statementID, currChoice.types, currChoice.condition, walk(currChoice.continuation))
            curScope = scopes(currChoice.label).parentScope.name
          }

        }

        if (solvedChoices.nonEmpty) {
          SendChoiceStatement(label, solvedChoices.toList)
        }
        else {
          null
        }

      case statement @ RecursiveStatement(label, body) =>
        RecursiveStatement(label, walk(statement.body))

      case statement @ RecursiveVar(name, continuation) =>
        checkRecVariable(scopes(curScope), statement)
        RecursiveVar(name, walk(statement.continuation))

      case End() => End()

    }
  }

  @scala.annotation.tailrec
  private def checkRecVariable(scope: Scope, recursiveVar: RecursiveVar): Scope = {
    if(scope != null){
      if(!scopes(scope.name).recVariables.contains(recursiveVar.name)){
        checkRecVariable(scopes(scope.name).parentScope, recursiveVar)
      } else {
        scope
      }
    } else {
      throw new Exception("Error: Recursive variable "+recursiveVar.name+" not defined.")
    }
  }

  private def createAndUpdateScope(label: String): Unit = {
    scopes(label) = new Scope(label, label, scopes(curScope))
    curScope = label
  }

  private def checkAndInitVariables(label: String, types: Map[String, String], condition: String): Unit = { // Expression
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

    override def traverse(tree: Tree): Unit = tree match {
      case i @ Ident(_) =>
        identifiers = i.name.decodedName.toString :: identifiers
        super.traverse(tree)
      case _ =>
        super.traverse(tree)
    }
  }

  def getIdentifiers(condition: String): List[String] = {

    val conditionTree = toolbox.parse(condition)

    val traverser = new traverser
    traverser.traverse(conditionTree)
    traverser.identifiers.distinct.filter(_ != "util")
  }

  def getAggIdentifiers(aggConds : List[String]) : List[String] = {
    val traverser = new traverser
    var identifiers = ListBuffer[String]()

    for (condition <- aggConds) {
      val conditionTree = toolbox.parse(condition)
      traverser.traverse(conditionTree)
      val identifiersList = traverser.identifiers.distinct.filter(_ != "util")
      for (ident <- identifiersList) {
        identifiers += ident
      }
    }
    identifiers.toList
  }

  def searchIdent(tmpCurScope: String, identifierName: String): String = {
    if(!scopes(tmpCurScope).variables.contains(identifierName)){
      if(scopes(tmpCurScope).parentScope==null){
        throw new Exception("STSolver - Identifier "+identifierName+" not in scope")
      }
      searchIdent(scopes(tmpCurScope).parentScope.name, identifierName)
    } else {
      tmpCurScope
    }
  }


  /**
   * Type checks a condition of type String using the scala compiler. First, the identifiers are extracted
   * from the condition. Their type is then retrieved and appended to a string as variable declarations. The contents
   * of the util file are extracted as string. The latter, the variable declarations and the condition itself
   * are all appended to a string which is parsed using the Scala parsers and then type-checked using the
   * Scala compiler.
   *
   * @param label The label of the current statement.
   * @param types A mapping from an identifier to its respective type (representing the payload of
   *              the current statement).
   * @param condition The condition to type-check.
   * @return The whether the condition is of type boolean or not.
   */

  private def checkCondition(label: String, types: Map[String, String], condition: String): Unit ={ // this shouldnt return bool, it should return the clauses
    if(condition != null) {
      var stringVariables = ""

      var tmpScope = curScope
      var aggConds = ""

      breakable {
        while (scopes(tmpScope).parentScope.name != "global") {
          tmpScope = scopes(tmpScope).parentScope.name
          if (scopes(tmpScope).trace != List()) {
            scopes(curScope).trace = (label, condition) :: scopes(tmpScope).trace
            break
          }
        }
        scopes(curScope).trace = List((label, condition))
      }
      aggConds = helper.aggCondsToString(scopes(curScope).getAssertions)

      val identifiersInAggConds = getIdentifiers(aggConds)

      // getting util file contents
      val source = scala.io.Source.fromFile(path+"/util.scala", "utf-8")
      var util = try source.mkString finally source.close()
      util = util.replaceFirst("package .*\n", "")
      util = util.replace(preamble, "")

      for(identName <- identifiersInAggConds){
        val identifier = scopes(searchIdent(curScope, identName)).variables(identName)
        stringVariables = stringVariables+"val "+identName+": "+identifier._2+"= ???;\n"
      }

      val stringAggConds = aggConds
      val eval = s"""
                    |$util
                    |$stringVariables
                    |$stringAggConds
                    |""".stripMargin
      val tree = toolbox.parse(eval)
      val checked = toolbox.typecheck(tree)
      checked.tpe == Boolean
    }
  }


  def solverUnopt(trace : List[(String, String)]) : Boolean = {
    var aggConds = helper.aggCondsToString(scopes(curScope).getAssertions)

    var variables : Map[String, String] = Map()
    val identifiersInAggConds = getAggIdentifiers(List(aggConds))
    for(identName <- identifiersInAggConds){
      val identifier = scopes(searchIdent(curScope, identName)).variables(identName)
      variables = variables + (identName -> identifier._2)
    }

    var (condTree, boolExpr, sat) = executeSolver(aggConds, variables)
    sat
  }

  def solver(trace : List[(String, String)]): Boolean ={

    val traceLabels = trace.map(event=>event._1)
    val currLabel = traceLabels.head
    val aggConds = trace.map(event=>event._2)
    val currCond = aggConds.head

    if((traceLabels != null) && (aggConds != null)) {
      var variables : Map[String, String] = Map()
      val identifiersInAggConds = getAggIdentifiers(aggConds)
      for(identName <- identifiersInAggConds){
        val identifier = scopes(searchIdent(curScope, identName)).variables(identName)
        variables = variables + (identName -> identifier._2)
      }

      // testing condition itself
      var unsatConds = ""
      var checkLemmasFirst = true
      if(solver.getLemmas.nonEmpty) {
        checkLemmasFirst = solver.compareToLemmas(helper.aggCondsToTree(List(currCond)))
      }

      if(checkLemmasFirst) {
        var (condTree, boolExpr, sat) = executeSolver(currCond, variables)
        addEvent(currLabel, boolExpr)
        if (!sat) {
          unsatConds = currCond
          solver.addLemma(Set(condTree))
          allTraces = helper.updateTraces(traceLabels, allTraces)
        }

        var tempCurrEvent: List[(String, String)] = List() // trace.head

        var fullTrace: List[BoolExpr] = List()

        if ((trace.length > 1) && sat) {
          var tempTrace = trace

          // first check if unsat for sure
          for (ev <- tempTrace) {
            fullTrace = getEvent(ev._1) :: fullTrace
          }

          val aggConds = solver.aggregate(fullTrace.to[ListBuffer])

          var checkLemmas = true
          if (solver.getLemmas.nonEmpty) {
            val fullTraceToTree = helper.aggCondsToTree(tempTrace.map(ev => ev._2))
            checkLemmas = solver.compareToLemmas(fullTraceToTree)
          }

          var matchLemma = false
          var satAgg = false
          if (checkLemmas)
            satAgg = executeSolverOnAgg(aggConds)
          else {
            matchLemma = true
            satAgg = false
          }

          if (!satAgg) {
            breakable {
              while (sat && (tempTrace.length > 1)) {
                tempCurrEvent = tempTrace.head :: tempCurrEvent
                tempTrace = tempTrace.tail

                for (event <- tempTrace) {
                  val aggCondsString = helper.aggCondsToString(event._2 :: tempCurrEvent.map(ev => ev._2))
                  var aggToTree: Set[scala.meta.Term] = Set()
                  var listOfBoolExprs: List[BoolExpr] = List()

                  for (ev <- tempCurrEvent) {
                    listOfBoolExprs = getEvent(ev._1) :: listOfBoolExprs
                  listOfBoolExprs = getEvent(event._1) :: listOfBoolExprs
                  val aggConds = solver.aggregate(listOfBoolExprs.to[ListBuffer])

                  var checkLemmas = true
                  if (solver.getLemmas.nonEmpty) {
                    aggToTree = helper.aggCondsToTree(event._2 :: tempCurrEvent.map(ev => ev._2))
                    checkLemmas = solver.compareToLemmas(aggToTree)
                  }
                  var matchLemma = false
                  if (checkLemmas)
                    sat = executeSolverOnAgg(aggConds)
                  else {
                    matchLemma = true
                    sat = false
                  }

                  if (!sat) {
                    if (!matchLemma) {
                      if (aggToTree.isEmpty)
                        aggToTree = helper.aggCondsToTree(event._2 :: tempCurrEvent.map(ev => ev._2))
                      solver.addLemma(aggToTree)
                    }
                    // updating trace status
                    allTraces = helper.updateTraces(traceLabels, allTraces)

                    break
                  }
                }
              }
            }
          }
          if (sat) {
            //println("THIS BRANCH IS REACHABLE")
          }
        }

        sat
      }
      else {
//        println("THIS BRANCH IS UNREACHABLE - IMMEDIATE LEMMA MISMATCH")
//        println("UNSATISFIABLE CONDITIONS")
//        println(currCond)

        // updating trace status
        allTraces = helper.updateTraces(traceLabels, allTraces)
        false
      }
    }
    else {
      true
    }
  }

  private def generateUtilFunctionModels(util : String) : Unit = {
    solver.generateUtilFunctions(util)
  }

  private def executeSolver(condition : String, variables : Map[String, String]) : (scala.meta.Term, BoolExpr, Boolean) = {
    val (condTree, condExpr) = solver.generateFormulas(condition, variables)
    if(solver.compareToLemmas(Set(condTree)))
      (condTree, condExpr, solver.checkUnsat())
    else {
      (condTree, condExpr, false)
    }
  }

  private def executeSolverOnAgg(aggConds : BoolExpr) : Boolean ={
    solver.solveAgg(aggConds)
  }


}
