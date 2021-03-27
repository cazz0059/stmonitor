package monitor.parser

import scala.collection.mutable
import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox
import monitor.model._
import monitor.model.Scope

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

//import z3

import com.typesafe.scalalogging.Logger

class STSolver(sessionType : SessionType, path: String){

  // modify original session type

  val logger: Logger = Logger("STSolver")

  private val toolbox = currentMirror.mkToolBox()

  private val helper = new STSolverHelper()

  private var scopes = new mutable.HashMap[String, Scope]()
  private var curScope = "global"
  scopes(curScope) = new Scope("global", null)

  private var solvedST : SessionType = new SessionType(null, null)

  //private var branches = new mutable.HashMap[Statement, String]()

  def getRecursiveVarScope(recursiveVar: RecursiveVar): Scope = {
    checkRecVariable(scopes(curScope), recursiveVar)
  }

  def getCurScope: String ={
    curScope
  }

  def getScope(scopeName: String): Scope = {
    scopes(scopeName)
  }

  def run() : SessionType = {
//    sessionType.statement match { // this part might all be useless
//      case recursiveStatement: RecursiveStatement =>
//        var tmpStatement: Statement = null
//        while(tmpStatement.isInstanceOf[RecursiveStatement]){
//          tmpStatement = recursiveStatement.body
//        }
//        checkStatement(recursiveStatement.body) // check if this is the right method, because in interpreter it is that
//                                                // monitor is initialised, not the whole statement checked
//      case _ =>
//        checkStatement(sessionType.statement) // same here
//    }

    initialWalk(sessionType.statement)
    curScope = "global"
    println()
    logger.info("Initial Walk Complete")
    println()

    solvedST.name = sessionType.name
    solvedST.statement = walk(sessionType.statement)
    solvedST//sessionType
  }

  def initialWalk(root: Statement): Unit = {
    root match {
      case ReceiveStatement(label, types, condition, continuation) =>
        createAndUpdateScope(label) // creates new scope
        checkAndInitVariables(label, types, condition) // puts vars in scopes
        handlePayloads(label, types) // synths variables in monitor
        initialWalk(continuation)

      case SendStatement(label, types, condition, continuation) =>
        createAndUpdateScope(label)
        checkAndInitVariables(label, types, condition)
        handlePayloads(label, types)
        initialWalk(continuation)

      case ReceiveChoiceStatement(label, choices) =>
        createAndUpdateScope(label)
        for(choice <- choices) {
          createAndUpdateScope(choice.asInstanceOf[ReceiveStatement].label)
          checkAndInitVariables(choice.asInstanceOf[ReceiveStatement].label, choice.asInstanceOf[ReceiveStatement].types, choice.asInstanceOf[ReceiveStatement].condition)
          handlePayloads(choice.asInstanceOf[ReceiveStatement].label, choice.asInstanceOf[ReceiveStatement].types)
          initialWalk(choice.asInstanceOf[ReceiveStatement].continuation)
          curScope = scopes(choice.asInstanceOf[ReceiveStatement].label).parentScope.name
        }

      case SendChoiceStatement(label, choices) =>
        createAndUpdateScope(label)
        for(choice <- choices) {
          createAndUpdateScope(choice.asInstanceOf[SendStatement].label)
          checkAndInitVariables(choice.asInstanceOf[SendStatement].label, choice.asInstanceOf[SendStatement].types, choice.asInstanceOf[SendStatement].condition)
          handlePayloads(choice.asInstanceOf[SendStatement].label, choice.asInstanceOf[SendStatement].types)
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

  // this one should do all the solving stuff while traversing
  def walk(statement: Statement): Statement = {
    statement match {
      case statement @ ReceiveStatement(label, types, condition, _) =>
        println()
        logger.info("Receive "+label+"("+types+")")
        println()
        curScope = label
        // get current conditions and make them into clauses
        // add them to the set of condition clauses by getting set in prev scope and adding current to these
        // check the satisfiability of the clauses
        // if redundant, move ot the next functions below
        checkCondition(label, types, condition)
        val payload = scopes(curScope).assertions

        // "rebuild parse tree" functions here
        //handleReceive(statement, statement.continuation)
        println("Verdict : " + payload)
        if(condition == null || solver(payload)) {
          ReceiveStatement(label, types, condition, walk(statement.continuation))
        }
        else {
          null
        }

      case statement @ SendStatement(label, types, condition, _) =>
        println()
        logger.info("Send "+label+"("+types+")")
        println()
        curScope = label
        checkCondition(label, types, condition)
        val payload = scopes(curScope).assertions
        println("Verdict : " + payload)
        if(condition == null || solver(payload)) {
          SendStatement(label, types, condition, walk(statement.continuation))
        }
        else {
          null
        }

        //handleSend(statement, statement.continuation)


      case statement @ ReceiveChoiceStatement(label, choices) =>
        println()
        logger.info("Receive Choice Statement "+label+"{"+choices+"}")
        println()
        curScope = label
        var solvedChoices : ListBuffer[Statement] = ListBuffer()
        //handleReceiveChoice(statement)

        for(choice <- choices) {
          val currChoice = choice.asInstanceOf[ReceiveStatement]
          curScope = currChoice.label
          checkCondition(currChoice.label, currChoice.types, currChoice.condition)
          val payload = scopes(curScope).assertions
          //synthProtocol.handleReceive(choice.asInstanceOf[ReceiveStatement], choice.asInstanceOf[ReceiveStatement].continuation, statement.label)

          println("Verdict : " + currChoice.condition)
          if (currChoice.condition == null || solver(payload)) {
            //val solvedChoice : List[Statement] = List(walk(choice.asInstanceOf[ReceiveStatement].continuation))
            solvedChoices += ReceiveStatement(currChoice.label, currChoice.types, currChoice.condition, walk(currChoice.continuation))
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
        println()
        logger.info("Send Choice Statement "+label+"{"+choices+"}")
        println()
        curScope = label
        var solvedChoices : ListBuffer[Statement] = ListBuffer()
        //handleSendChoice(statement)

        for(choice <- choices) {
          val currChoice = choice.asInstanceOf[SendStatement]
          curScope = currChoice.label
          checkCondition(currChoice.label, currChoice.types, currChoice.condition)
          val payload = scopes(curScope).assertions

          //synthProtocol.handleSend(choice.asInstanceOf[SendStatement], choice.asInstanceOf[SendStatement].continuation, statement.label)
          println("Verdict : " + currChoice.condition)
          if (currChoice.condition == null || solver(payload)) {
            solvedChoices += SendStatement(currChoice.label, currChoice.types, currChoice.condition, walk(currChoice.continuation))
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
        println()
        logger.info("Recursive statement with variable "+label+" and body: " +body)
        println()
        RecursiveStatement(label, walk(statement.body))

      case statement @ RecursiveVar(name, continuation) =>
        println()
        logger.info("Recursive variable "+name)
        println()
        checkRecVariable(scopes(curScope), statement)
        RecursiveVar(name, walk(statement.continuation))

      case End() => null

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
    scopes(label) = new Scope(label, scopes(curScope))
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

    // post exams notes
    // this currently just gets current condition. i need to store all conditions of a trace

    print("~~~ CONDITION TREE ~~~\n")
    print(" - Normal\n")
    print(show(conditionTree))
    print("\n - Raw\n")
    print(showRaw(conditionTree))
    print("\n")

    val traverser = new traverser
    traverser.traverse(conditionTree)
    //print("\n" ++ traverser. ++ "\n\n") // see what more i can do with this traversed tree
    // i can do this after i allow traverse to have more than identifiers as an attribute
    // print clauses here instead of just list of idents
    print(" - To Return:\n" ++ traverser.identifiers.distinct.filter(_ != "util").toString() ++ "\n\n")

    // maybe all the above can be done using the new models implemented?
    traverser.identifiers.distinct.filter(_ != "util")
  }

  def searchIdent(tmpCurScope: String, identifierName: String): String = {
    println("///")
    println("Identifier name: " + identifierName)
    println("Temp Cur Scope: " + tmpCurScope)
    if(!scopes(tmpCurScope).variables.contains(identifierName)){
      if(scopes(tmpCurScope).parentScope==null){
        println("Parent Scope: NULL")
        throw new Exception("STSolver - Identifier "+identifierName+" not in scope")
      }
      println("Parent Scope: " + scopes(tmpCurScope).parentScope.name)
      println("###")
      searchIdent(scopes(tmpCurScope).parentScope.name, identifierName)
    } else {
      println("###")
      tmpCurScope
    }
  }

  // this function is called by the initial walk
  // it saves payloads (in monitor)
  // idk if i keep this or not
  // re-evaluate structure of function calling
  def handlePayloads(label: String, types: Map[String, String]): Unit ={ // arrange this so that it checks payloads
    val mon = new StringBuilder()
    mon.append("\t\tobject "+label+" {\n")
    for(typ <- types){
      mon.append("\t\t\tvar "+typ._1+": "+typ._2+" = _\n")
    }
    mon.append("\t\t}\n")
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

      println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      println("Parent scope: " + scopes(curScope).parentScope.name)
      println("Parent scope Payload: " + scopes(curScope).parentScope.assertions)

      var tmpScope = curScope

      breakable {
        while (scopes(tmpScope).parentScope.name != "global") {
          tmpScope = scopes(tmpScope).parentScope.name
          if (scopes(tmpScope).assertions != "") {
            scopes(curScope).assertions = "(" + scopes(tmpScope).assertions + ") && (" + condition + ")"
            break
          }
        }
        scopes(curScope).assertions = condition
      }

      //      if (scopes(tmpScope).parentScope.assertions == "") {
      //        scopes(curScope).assertions = condition
      //      }
      //      else {
      //        scopes(curScope).assertions = "(" + scopes(curScope).parentScope.assertions + ") && (" + condition + ")"
      //      }

      val payload = scopes(curScope).assertions
      val identifiersInPayload = getIdentifiers(payload) // will become getClauses
      println("Current scope: " + curScope)
      println("Idents in Payload: " + identifiersInPayload)

      // getting util file contents
      val source = scala.io.Source.fromFile(path+"/util.scala", "utf-8")
      val util = try source.mkString finally source.close()

      for(identName <- identifiersInPayload){
        val identifier = scopes(searchIdent(curScope, identName)).variables(identName)
        stringVariables = stringVariables+"val "+identName+": "+identifier._2+"= ???;\n"
      }



      val stringPayload = payload//condition //helper.conditionToString(condition)
      println("Current scope: " + curScope)
      println("Payload: " + stringPayload)

      //println("\n ~ Util >>>\n " ++ util ++ "\n<<<")
      //println("\n ~ String Variables >>>\n " ++ stringVariables ++ "\n<<<")
      //println("\n ~ String Condition >>>\n " ++ stringCondition ++ "\n<<<")

      val eval = s"""
                    |$util
                    |$stringVariables
                    |$stringPayload
                    |""".stripMargin
      val tree = toolbox.parse(eval) // research on what toolbox does and what these functions do
      println("\n ~ Tree >>>\n " ++ tree.toString() ++ "\n<<<")
      val checked = toolbox.typecheck(tree)
      //println("\n ~ Checked >>>\n " ++ checked.toString() ++ "\n<<<")
      checked.tpe == Boolean
    }
  }

//  def checkStatement(statement: Statement) : Unit = {
//
//  }

  def solver(conditions : String): Boolean ={
    println(" ::::::::::::::::: SOLVER ::::::::::::::::::::")

    if(conditions != null) {
      println("\n ~ Conditions >>>\n " ++ conditions ++ "\n<<<")
      val parsedConditions = toolbox.parse(conditions)
      println("\n ~ Parsed Conditions >>>\n " ++ parsedConditions.toString() ++ "\n<<<")

      // getting util file contents
      val source = scala.io.Source.fromFile(path + "/util.scala", "utf-8")
      val util = try source.mkString finally source.close()

      // need to parse conditions
      val parsedUtil = toolbox.parse(util)
      println("\n ~ Parsed Util >>>\n " ++ parsedUtil.toString() ++ "\n<<<")
      //val cnf = helper.getCurrentConditions(condition)
      //helper.cnfToString(cnf)

      // after checking type do the things to do for condition calculating and saving
      // no what, there s nothing to calculate
      // see steps in evernote
      //solver() // solver goes in here

      // user input for now
      println("Is this SAT?")
      val ans = scala.io.StdIn.readLine()
      if (ans == "no") {
        println("Answer is no, so UNSAT")
        false
      }
      else {
        true
      }
    }
    else {
      true
    }

    //val z3 = new Z3Context("MODEL" -> true)
    // this returns sat/unsat?
  }

//  def rebuildSessionType(statement: Statement) : Statement = {
//    statement match {
//      case statement@ReceiveStatement(label, types, condition, _) =>
//
//      case statement@SendStatement(label, types, condition, _) =>
//
//      case statement@ReceiveChoiceStatement(label, choices) =>
//
//
//      case statement@SendChoiceStatement(label, choices) =>
//    }
//  }


  // REBUILDING PARSE TREE
  // check out synthmon/synthprotocol setup for these functions
  def handleReceive(statement: ReceiveStatement, nextStatement: Statement): Unit = {

  }

  def handleSend(statement: SendStatement, nextStatement: Statement): Unit = {

  }

  def handleReceiveChoice(statement: ReceiveChoiceStatement): Unit = {

  }

  def handleSendChoice(statement: SendChoiceStatement): Unit ={

  }

}