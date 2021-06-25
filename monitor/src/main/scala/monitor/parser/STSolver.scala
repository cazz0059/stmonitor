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

//  def pause() : Unit = {
//    print("Pausing...")
//    scala.io.StdIn.readLine()
//  }

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
      println("ERROR: labelled event not found"); null // cannot be accessed
  }


  def run() : SessionType = {
//    println(allTraces)
//    pause()


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
        // get current conditions and make them into clauses
        // add them to the set of condition clauses by getting set in prev scope and adding current to these
        // check the satisfiability of the clauses
        // if redundant, move ot the next functions below
        checkCondition(label, types, condition)
        val trace = scopes(curScope).trace

        if(condition == null || solver(trace)) {
          ReceiveStatement(label, id, types, condition, walk(statement.continuation))
        }
        else {
          null
        }

      case statement @ SendStatement(label, id, types, condition, _) =>
        //logger.info("Send "+label+"("+types+")")
        curScope = label
        checkCondition(label, types, condition)
        val trace = scopes(curScope).trace
        //println("Verdict : " + scopes(curScope).getAssertions)
        if(condition == null || solver(trace)) {
          SendStatement(label, id, types, condition, walk(statement.continuation))
        }
        else {
          null
        }

        //handleSend(statement, statement.continuation)


      case statement @ ReceiveChoiceStatement(label, choices) =>
        //logger.info("Receive Choice Statement "+label+"{"+choices+"}")
        curScope = label
        var solvedChoices : ListBuffer[Statement] = ListBuffer()
        //handleReceiveChoice(statement)

        for(choice <- choices) {
          val currChoice = choice.asInstanceOf[ReceiveStatement]
          curScope = currChoice.label
          checkCondition(currChoice.label, currChoice.types, currChoice.condition)
          val trace = scopes(curScope).trace
          //synthProtocol.handleReceive(choice.asInstanceOf[ReceiveStatement], choice.asInstanceOf[ReceiveStatement].continuation, statement.label)

          //println("Verdict : " + scopes(curScope).getAssertions)
          if (currChoice.condition == null || solver(trace)) {
            //val solvedChoice : List[Statement] = List(walk(choice.asInstanceOf[ReceiveStatement].continuation))
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
        //logger.info("Send Choice Statement "+label+"{"+choices+"}")
        curScope = label
        var solvedChoices : ListBuffer[Statement] = ListBuffer()
        //handleSendChoice(statement)

        for(choice <- choices) {
          val currChoice = choice.asInstanceOf[SendStatement]
          curScope = currChoice.label
          checkCondition(currChoice.label, currChoice.types, currChoice.condition)
          val trace = scopes(curScope).trace

          //synthProtocol.handleSend(choice.asInstanceOf[SendStatement], choice.asInstanceOf[SendStatement].continuation, statement.label)
          //println("Verdict : " + scopes(curScope).getAssertions)
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
        //logger.info("Recursive statement with variable "+label+" and body: " +body)
        RecursiveStatement(label, walk(statement.body))

      case statement @ RecursiveVar(name, continuation) =>
        //logger.info("Recursive variable "+name)
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
    if (condition != null){ // .terms.nonEmpty // replace assertion function with actual assertion, basically build clauses not just identifiers
      // change scope class to hold clauses as well
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

    // with reference to above function, this should return the identifiers as a clause rather than just the identifiers
    override def traverse(tree: Tree): Unit = tree match { // change this so it doesnt only return identifiers?
      case i @ Ident(_) =>
        identifiers = i.name.decodedName.toString :: identifiers
        super.traverse(tree)
      case _ =>
        super.traverse(tree)
    }
  }

//  def getIds(condition : Expression) : List[String] = {
//    var vars = getVars(condition)
//    var identifiers = List[String]()
//    for (v <- vars) {
//      identifiers = List.concat(identifiers, getIdentifiers(v))
//    }
//    identifiers
//  }
//
//  def getVars(expression : Expression) : List[String] = {
//    var vars = List[String]()
//    for (term <- expression.terms) {
//      for (not_factor <- term.not_factors) {
//        not_factor.factor match {
//          case Expression(terms) =>
//            vars = List.concat(vars, getVars(Expression(terms)))
//          case Variable(name) =>
//            vars ::= name
//        }
//      }
//    }
//    vars
//  }

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


  // typechecks condition
  // since this class will happen before(or during) interpreter, the conditions must be typechecked first before solving
  // debug this function to see what each step really does
  // was returning boolean, returning unit for now
  private def checkCondition(label: String, types: Map[String, String], condition: String): Unit ={ // this shouldnt return bool, it should return the clauses
    // make this function just a type checker
    if(condition != null) { // .terms.nonEmpty
      var stringVariables = ""

//      println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
//      println("Parent scope: " + scopes(curScope).parentScope.name)
//      println("Parent scope AggConds: " + scopes(curScope).parentScope.getAssertions)

      var tmpScope = curScope
      var aggConds = ""

      //solver.compareToLemmas("hello") // why is this here?

      breakable {
        while (scopes(tmpScope).parentScope.name != "global") {
          tmpScope = scopes(tmpScope).parentScope.name
//          println("TempScope: " + tmpScope)
          if (scopes(tmpScope).trace != List()) { // .assertions
//            println(" # Adding to list")
//            println(label + " : " + condition)
//            println(scopes(tmpScope).trace.toString())
            scopes(curScope).trace = (label, condition) :: scopes(tmpScope).trace
            //aggConds = helper.aggCondsToString(condition :: scopes(tmpScope).assertions)//"(" + scopes(tmpScope).assertions + ") && (" + condition + ")"
            break
          }
        }
//        println(" # The only condition")
        scopes(curScope).trace = List((label, condition))
      }
      aggConds = helper.aggCondsToString(scopes(curScope).getAssertions)

      //      if (scopes(tmpScope).parentScope.assertions == "") {
      //        scopes(curScope).assertions = condition
      //      }
      //      else {
      //        scopes(curScope).assertions = "(" + scopes(curScope).parentScope.assertions + ") && (" + condition + ")"
      //      }

      //val aggConds = scopes(curScope).assertions
      val identifiersInAggConds = getIdentifiers(aggConds) // will become getClauses <- WHAT
//      println("Current scope: " + curScope)
//      println("Idents in Payload: " + identifiersInAggConds)

      // getting util file contents
      val source = scala.io.Source.fromFile(path+"/util.scala", "utf-8")
      var util = try source.mkString finally source.close()
//      println("\n ~ Util1 >>>\n " ++ util ++ "\n<<<")
      util = util.replaceFirst("package .*\n", "")
      util = util.replace(preamble, "")
      //println("[]")
      //print(util)
      //println("[]")

      for(identName <- identifiersInAggConds){
        val identifier = scopes(searchIdent(curScope, identName)).variables(identName)
        stringVariables = stringVariables+"val "+identName+": "+identifier._2+"= ???;\n"
      }

      val stringAggConds = aggConds//condition //helper.conditionToString(condition)
//      println("Current scope: " + curScope)
//      println("AggConds: " + stringAggConds)

//      println("\n ~ Util >>>\n " ++ util.toString ++ "\n<<<")
//      println("\n ~ String Variables >>>\n " ++ stringVariables ++ "\n<<<")
//      println("\n ~ String Condition >>>\n " ++ stringAggConds ++ "\n<<<")

      val eval = s"""
                    |$util
                    |$stringVariables
                    |$stringAggConds
                    |""".stripMargin
      val tree = toolbox.parse(eval) // research on what toolbox does and what these functions do
      //println("\n ~ Tree >>>\n " ++ tree.toString() ++ "\n<<<")
      val checked = toolbox.typecheck(tree)
      //println("\n ~ Checked >>>\n " ++ checked.toString() ++ "\n<<<")
      checked.tpe == Boolean
    }
  }

//  def checkStatement(statement: Statement) : Unit = {
//
//  }

  def solverUnopt(trace : List[(String, String)]) : Boolean = {
    var aggConds = helper.aggCondsToString(scopes(curScope).getAssertions)

    var variables : Map[String, String] = Map()
    val identifiersInAggConds = getAggIdentifiers(List(aggConds))
    for(identName <- identifiersInAggConds){
      val identifier = scopes(searchIdent(curScope, identName)).variables(identName)
      //println("val "+identName+": "+identifier._2+"= ???;")
      variables = variables + (identName -> identifier._2)
    }

    var (condTree, boolExpr, sat) = executeSolver(aggConds, variables)
    sat
  }

  def solver(trace : List[(String, String)]): Boolean ={ // currentCond : String, traceLabels : List[String], aggConds : List[String]
    //println(" ::::::::::::::::: SOLVER ::::::::::::::::::::")

    val traceLabels = trace.map(event=>event._1)
    val currLabel = traceLabels.head
    val aggConds = trace.map(event=>event._2)
    val currCond = aggConds.head

    if((traceLabels != null) && (aggConds != null)) {
//      println("\n ~ Trace >>>\n " ++ traceLabels.toString() ++ "\n<<<")
//      println("\n ~ Conditions >>>\n " ++ aggConds.toString() ++ "\n<<<")
//      pause()
      // this is in the solver itself, no need
//      val parsedConditions = toolbox.parse(conditions)
//      println("\n ~ Parsed Conditions >>>\n " ++ parsedConditions.toString() ++ "\n<<<")

      var variables : Map[String, String] = Map()
      val identifiersInAggConds = getAggIdentifiers(aggConds)
      for(identName <- identifiersInAggConds){
        val identifier = scopes(searchIdent(curScope, identName)).variables(identName)
        //println("val "+identName+": "+identifier._2+"= ???;")
        variables = variables + (identName -> identifier._2)
      }



      // need to parse conditions
//      val parsedUtil = toolbox.parse(util)
//      println("\n ~ Parsed Util >>>\n " ++ parsedUtil.toString() ++ "\n<<<")
      //val cnf = helper.getCurrentConditions(condition)
      //helper.cnfToString(cnf)

      // after checking type do the things to do for condition calculating and saving
      // no what, there s nothing to calculate
      // see steps in evernote
      //solver() // solver goes in here


      // 21/04/2021
      // IMPROVEMENT OF THIS LOOP - TO DO
      // - save the label of the event and its corresponding parsed context assertion, the whole set of commands
      // - instead of parsing the conditions each time, they are only parsed once and then can access the context assertions from the map described above
      // - the order of the events can be saved through a hashmap or throughout the looping
      // var eventAssertions : Map[String, ]
      // not too sure how i am going to execute this
      // initial thought was to use model like utilFuncs
      // but there is the confusion of variable decl
      // its doable, since there r no declarations in the model
      // im going to test out functions and utilFuncs first, and then do that later, its easier this way coz im not sure the utilFuncs method even works

      //val currentCond = aggConds.head
      var unsatConds = ""
      //println(" - Testing condition itself")
      var checkLemmasFirst = true
      if(solver.getLemmas.nonEmpty) {
        checkLemmasFirst = solver.compareToLemmas(helper.aggCondsToTree(List(currCond)))
      }

      if(checkLemmasFirst) {
        var (condTree, boolExpr, sat) = executeSolver(currCond, variables)
        addEvent(currLabel, boolExpr)
        //println(" - Done")
        if (!sat) {
          unsatConds = currCond
          solver.addLemma(Set(condTree))
          // add to lemmas --------------------------------------------------------------------------------
          allTraces = helper.updateTraces(traceLabels, allTraces)
        }

        var tempCurrEvent: List[(String, String)] = List() // trace.head

        var fullTrace: List[BoolExpr] = List()

        if ((trace.length > 1) && sat) {
          //println("################# First cond checked")
          var tempTrace = trace

          // first check if unsat for sure
          for (ev <- tempTrace) {
            //println("Checking event is correct... " + ev._1 + " : " + getEvent(ev._1))
            fullTrace = getEvent(ev._1) :: fullTrace
          }

          val aggConds = solver.aggregate(fullTrace.to[ListBuffer])

          var checkLemmas = true
          if (solver.getLemmas.nonEmpty) { // avoids unnecessary scala meta term parsing
            val fullTraceToTree = helper.aggCondsToTree(tempTrace.map(ev => ev._2))
            checkLemmas = solver.compareToLemmas(fullTraceToTree)
          }
          //println("Checked lemmas")
          //pause()
          var matchLemma = false
          var satAgg = false
          if (checkLemmas)
            satAgg = executeSolverOnAgg(aggConds)
          else {
            //println("Found equal lemma")
            matchLemma = true
            satAgg = false
          }

          if (!satAgg) {
            breakable {
              while (sat && (tempTrace.length > 1)) {
                //println("################# and satisfiable")
                tempCurrEvent = tempTrace.head :: tempCurrEvent
                //println("tempCurrEvent : " + tempCurrEvent)
                //println("################## Hence we proceed")
                tempTrace = tempTrace.tail

                for (event <- tempTrace) { // for each event in the rest of the trace
                  val aggCondsString = helper.aggCondsToString(event._2 :: tempCurrEvent.map(ev => ev._2)) // gets a string of conjunction of conditions
                  var aggToTree: Set[scala.meta.Term] = Set() // helper.aggCondsToTree(event._2 :: tempCurrEvent.map(ev=>ev._2))
                  //println("##################### NOW TESTING CONDITIONS:")
                  //println(aggCondsString) // wat was i thinking, this doesnt keep looping like i want it to
                  var listOfBoolExprs: List[BoolExpr] = List()

                  for (ev <- tempCurrEvent) {
                    //println("Checking event is correct... " + ev._1 + " : " + getEvent(ev._1))
                    listOfBoolExprs = getEvent(ev._1) :: listOfBoolExprs
                  } // getting "first" current conditions
                  listOfBoolExprs = getEvent(event._1) :: listOfBoolExprs // getting the one from rest of trace
                  val aggConds = solver.aggregate(listOfBoolExprs.to[ListBuffer])

                  var checkLemmas = true
                  if (solver.getLemmas.nonEmpty) { // avoids unnecessary scala meta term parsing
                    aggToTree = helper.aggCondsToTree(event._2 :: tempCurrEvent.map(ev => ev._2))
                    checkLemmas = solver.compareToLemmas(aggToTree)
                  }
                  var matchLemma = false
                  if (checkLemmas)
                    sat = executeSolverOnAgg(aggConds)
                  else {
                    //println("Found equal lemma")
                    matchLemma = true
                    sat = false
                  }
                  //println("##################### Tested conditions")
                  if (!sat) {
//                    println("THIS BRANCH IS UNREACHABLE")
//                    println("UNSATISFIABLE CONDITIONS")
//                    println(aggCondsString)
                    if (!matchLemma) {
                      if (aggToTree.isEmpty) // creates the tree if not yet created - "avoids unnecessary scala meta term parsing"
                        aggToTree = helper.aggCondsToTree(event._2 :: tempCurrEvent.map(ev => ev._2))
                      solver.addLemma(aggToTree)
                    }
                    // updating trace status
                    allTraces = helper.updateTraces(traceLabels, allTraces)

                    break
                    // add to lemmas
                  }
                }
                //            if (tempAggConds.length > 1) {
                //              tempCurrCond = helper.aggCondsToString(tempCurrCond :: List(tempAggConds.head))
                //            }
                //            else {
                //              println("############ Have to stop since all conditions tested")
                //              break
                //            }

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


      // user input for now
//      println("Is this SAT?")
      //println("Current lemmas -")
      //println(solver.getLemmas)
      //pause()

//      if(!sat) {
//
//      }

      //sat // ----------------------------------------------------------------------------------------

//      if (ans == "no") {
//        println("Answer is no, so UNSAT")
//        false
//      }
//      else {
//        true
//      }
    }
    else {
      true
    }

    //val z3 = new Z3Context("MODEL" -> true)
    // this returns sat/unsat?
  }

  private def generateUtilFunctionModels(util : String) : Unit = {
    solver.generateUtilFunctions(util)
  }

  private def executeSolver(condition : String, variables : Map[String, String]) : (scala.meta.Term, BoolExpr, Boolean) = {
    //println("EXECUTING SOLVER ##")
    val (condTree, condExpr) = solver.generateFormulas(condition, variables)
    //val outputStream = solver.processInput(solver.convertConditionsToSMTLIB(smtlibFormat))
    if(solver.compareToLemmas(Set(condTree)))
      (condTree, condExpr, solver.checkUnsat())
    else {
      //println("equal lemma found")
      (condTree, condExpr, false)
    }
  }

  private def executeSolverOnAgg(aggConds : BoolExpr) : Boolean ={
    //println("EXECUTING SOLVER ##")
    solver.solveAgg(aggConds)
  }


}
