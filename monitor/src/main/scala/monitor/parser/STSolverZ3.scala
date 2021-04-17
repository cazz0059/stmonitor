package monitor.parser

import smtlib.trees.Commands._
import scala.collection.mutable.ListBuffer

import com.microsoft.z3._

import java.io._

//import scala.reflect.runtime._
//import scala.reflect.runtime.universe._
//import scala.tools.reflect.ToolBox

import scala.meta._

import scala.sys.process._

//import z3

class STSolverZ3 {

  private lazy val z3_path = {
    sys.env.getOrElse("Z3_EXE", "z3.exe")
  }

  //private val toolbox = currentMirror.mkToolBox()

  //private var smtlibVariables = ""
  private var variablesInt : Map[String, IntExpr] = Map()
  private var variablesBool : Map[String, BoolExpr] = Map()

  private var utilFuncs : Map[String, Model] = Map()
  private var utilCtx : Map[String, Context] = Map()

  private val ctx: Context = new Context(new java.util.HashMap[String, String])

  // find way to eliminate conditions that have nothing to do with the unsatisfiability - saveUnsatConds()
  private var lemmas : List[String] = List() // SMT read this and see if it is the same semantics
  //private val cnfTransformer = new TransformToCnf()

  def pause() : Unit = {
    scala.io.StdIn.readLine()
  }

  def createContext(funcName : String) : Unit = {
    utilCtx = utilCtx + (funcName -> new Context(new java.util.HashMap[String, String]))
  }

  def addLemma(aggConds : String): Unit = {
    lemmas = aggConds :: lemmas
  }

  def compareToLemmas(aggConds : String): Unit = {
    // can be done with toolbox - no use cnf instead, I think it saves easier

    // get semantics of aggConds - cnf clauses
    // for each lemma
    //    get semantics of lemma
    //    compare semantics - check if clauses in lemma make up subset of clauses in aggConds


  }

//  def saveUnsatConds(unsatCNF : CNF) : CNF = {
//    // for each combination of clauses
//    //    check which part of that clause is unsat (using z3), and return that one
//    //    if found the unsat part, no need to proceed with the loop
//    // start with the smallest combinations - optimisation
//    // include always the last condition/clause added as part of the unsat - optimisation
//    unsatCNF
//  }

  object traverser extends Traverser {
    // check if it traverses
    // make it add to the string
    //private var smtlibString : String = ""
    private var solver: com.microsoft.z3.Solver = ctx.mkSolver()
    def getSolver: com.microsoft.z3.Solver = {
      solver
    }
    def clearSolver() : Unit = {
      solver = ctx.mkSolver()
    }

    private var utilSolver: Map[String, com.microsoft.z3.Solver] = Map()
    def createUtilSolver(name : String) : Unit = {
      utilSolver = utilSolver + (name -> utilCtx(name).mkSolver())
    }
    def getUtilSolver(name : String): com.microsoft.z3.Solver = {
      utilSolver(name)
    }
    def clearUtilSolver() : Unit = {
      utilSolver = Map()
    }

//    def traverse(tree: Tree): Unit =
//      tree match {
//        case Term.Name(name) =>
//          // probably is a singular boolean variable, as in any other case it would form part of a formula
//          println("Term name " + name + " ##")
//          solver.add(variablesBool(name))
//        case Term.ApplyInfix(lhs, Term.Name(op), tArgs, args) =>
//          // has to be recursive over the term names
//          println("Term apply infix " + lhs.toString() + " " + op + " " + args.head.toString())
//          if(args.length > 1) {
//            println("Note: There are more args in this infix")
//          }
//
//          // variablesBool(lhs.toString())
//          op match {
//            case "&&" =>
//              var lhsBool = traverse(lhs)
//              for (arg <- args)
//                lhsBool = ctx.mkAnd(lhsBool, variablesBool(arg.toString()))
//              solver.add(lhsBool)
//            case "||" =>
//              var lhsBool = variablesBool(lhs.toString())
//              for (arg <- args)
//                lhsBool = ctx.mkOr(lhsBool, variablesBool(arg.toString()))
//              solver.add(lhsBool)
//            case "+" =>
//              var lhsInt = ctx.mkAdd(variablesInt(lhs.toString()), ctx.mkIntConst("0"))
//              for (arg <- args)
//                lhsInt = ctx.mkAdd(lhsInt, variablesInt(arg.toString()))
//              solver.add(lhsInt)
//            case "-" =>
//              var lhsInt = ctx.mkAdd(variablesInt(lhs.toString()), ctx.mkIntConst("0"))
//              for (arg <- args)
//                lhsInt = ctx.mkSub(lhsInt, variablesInt(arg.toString()))
//              solver.add(lhsInt)
//            case "*" =>
//              var lhsInt = ctx.mkAdd(variablesInt(lhs.toString()), ctx.mkIntConst("0"))
//              for (arg <- args)
//                lhsInt = ctx.mkMul(lhsInt, variablesInt(arg.toString()))
//              solver.add(lhsInt)
//            case "/" =>
//              var lhsInt = ctx.mkAdd(variablesInt(lhs.toString()), ctx.mkIntConst("0"))
//              for (arg <- args)
//                lhsInt = ctx.mkDiv(lhsInt, variablesInt(arg.toString()))
//              solver.add(lhsInt)
//          }



//      case Apply(fun, args) => // and are "ignored"? since the addition of the assert statements make up the conjunction
//        println("Apply " + fun.toString() + " args: " + args.toString() + " ##")
////        if (fun.toString().contains("util.")) {
////          smtlibVariables = smtlibVariables + "(declare-fun " + fun.toString() + " Bool)\n"
////        }
////        else {
//          super.traverse(fun)
//          super.traverseTrees(args)
////        }
//      case Ident(name) => // is this only accessed when the identifier is alone? check
//        println("Ident " + name + " ##")
//        smtlibString = smtlibString + "(assert " + name + ")\n"
//      case Select(ident, op) => // til now only accessed when there us unary, ands are accessed by apply
//        println("Select " + ident.toString() + " operator: " + op.toString + " ##")
//        var tmpOp = op.toString
//        tmpOp = tmpOp.replace("unary_$bang", "not ")
//        smtlibString = smtlibString + "(assert (" + tmpOp + ident.toString() + " ))\n"
//      case List(elts) => // when is this accessed
//        println("List " + elts.toString + " ##")
//      case Block(arg1, arg2) => // for util functions
//        println("Block:")
//        println(arg1.toString())
//        println("##")
//        println(arg2.toString())
//        println("##")
//        case _ =>
//          println("Other ##")
//  //        super.traverse(tree)
//    }

    // these return all possible combinations of a desired type
    def getBoolExpr(arg : Term, context: Context) : BoolExpr = arg match {
      case Lit.Boolean(value) =>
        val temp_bool_const =  context.mkBoolConst("temp_traverse_bool_const")
        var temp_and : BoolExpr = null

        if(!value) temp_and = context.mkAnd(temp_bool_const, context.mkNot(temp_bool_const))
        else temp_and = temp_bool_const

        temp_and

      case Term.Name(name) => traverseBoolVar(Term.Name(name), context)

      case Term.ApplyInfix(a, b, c, d) => traverseBoolInfix(Term.ApplyInfix(a, b, c, d), context)

      case Term.ApplyUnary(o, a) => traverseBoolUnary(Term.ApplyUnary(o, a), context)

      case _ =>
        println("TERM TYPE MISMATCH : BoolExpr")
        null
    }

    def getIntExpr(arg : Term, context: Context) : IntExpr = arg match {
      case Term.Name(name) => traverseIntVar(Term.Name(name), context)
      case _ =>
        println("TERM TYPE MISMATCH : IntExpr")
        null
    }

    def getArithExpr(arg : Term, context: Context) : ArithExpr[IntSort] = arg match {
      case Term.ApplyInfix(a, b, c, d) => traverseArithInfix(Term.ApplyInfix(a, b, c, d), context)
      case _ =>
        println("TERM TYPE MISMATCH : ArithExpr")
        null
    }

    def addFuncCall(name : String, solv : Solver) : Unit = {
      val func = utilFuncs(name)
      // creating new scope for function
      // issue when scoping if there are vars in conds and in util func with same name
      // another limitation : util functions are all of type bool
      solv.push()
      func match {
        case boolFunc @ BoolExpr =>
          solv.add(boolFunc)
        case _ =>
          println("Util function not boolean type")
          pause()
      }
      solv.pop()
    }

    // remember that embedded operations exist, so i cant just put "assert" in front of all the operators
    //    try and build the conditions (select and apply) recursively before adding the assert
    // use smt-lib List (recursive) some combination of that to unfold a recursive function???
    // or Tree??
    // z3 cannot prove by induction, so unfolding needs to take place to prove by deduction
    def traverseBoolVar(term : Term.Name, context: Context) : BoolExpr = term match {
      // probably is a singular boolean variable, as in any other case it would form part of a formula
      case Term.Name(name) =>
        println("Term name " + name + " ##")
        variablesBool(name)
        //solver.add(variablesBool(name))
    }

    def traverseIntVar(term : Term.Name, context: Context) : IntExpr = term match {
      // probably is a singular boolean variable, as in any other case it would form part of a formula
      case Term.Name(name) =>
        println("Term name " + name + " ##")
        variablesInt(name)
      //solver.add(variablesBool(name))
    }

    def traverseBoolInfix(term : Term.ApplyInfix, context: Context) : BoolExpr = term match {
      case Term.ApplyInfix(lhs, Term.Name("&&"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " && " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsBool = getBoolExpr(lhs, context)
        for (arg <- args)
          lhsBool = context.mkAnd(lhsBool, getBoolExpr(arg, context))
        lhsBool

      case Term.ApplyInfix(lhs, Term.Name("||"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " || " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsBool = getBoolExpr(lhs, context)
        for (arg <- args)
          lhsBool = context.mkOr(lhsBool, getBoolExpr(arg, context))
        lhsBool
    }

    def traverseArithInfix(term : Term.ApplyInfix, context: Context) : ArithExpr[IntSort] = term match {
      case Term.ApplyInfix(lhs, Term.Name("+"), tArgs, args) =>
        // has to be recursive over the term names
        println("Term apply infix " + lhs.toString() + " + " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsInt = context.mkAdd(getIntExpr(lhs, context)) // ctx.mkIntConst("0")
        for (arg <- args)
          lhsInt = context.mkAdd(lhsInt, getIntExpr(arg, context))
        lhsInt

      case Term.ApplyInfix(lhs, Term.Name("-"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " - " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsInt = context.mkAdd(getIntExpr(lhs, context))
        for (arg <- args)
          lhsInt = context.mkSub(lhsInt, getIntExpr(arg, context))
        lhsInt

      case Term.ApplyInfix(lhs, Term.Name("*"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " * " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsInt = context.mkAdd(getIntExpr(lhs, context))
        for (arg <- args)
          lhsInt = context.mkMul(lhsInt, getIntExpr(arg, context))
        lhsInt

      case Term.ApplyInfix(lhs, Term.Name("/"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " / " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsInt = context.mkAdd(getIntExpr(lhs, context))
        for (arg <- args)
          lhsInt = context.mkDiv(lhsInt, getIntExpr(arg, context))
        lhsInt

    }

    def traverseBoolUnary(term : Term.ApplyUnary, context: Context) : BoolExpr = term match {
      case Term.ApplyUnary(null, arg) =>
        getBoolExpr(arg, context)
      case Term.ApplyUnary(op, arg) =>
        context.mkNot(getBoolExpr(arg, context))
    }

    def traverseIntUnary(term : Term.ApplyUnary, context: Context) : ArithExpr[IntSort] = term match {
      case Term.ApplyUnary(null, arg) =>
        getIntExpr(arg, context)
      case Term.ApplyUnary(op, arg) =>
        println("Term unary " + arg)
        context.mkSub(null, getIntExpr(arg, context)) // ctx.mkBVNeg(ctx.mkInt2BV(getIntExpr(arg)))
    }

    // called only from conditions, not util file
    def traverseFuncBool(app : Term.Apply, context : Context) : Unit = app match { // does the adding itself
      case app @ Term.Apply(Term.Select(Term.Name("util"), Term.Name(name)), args) =>
        // this is function call within util function
        if (utilFuncs(name) != null) {
          addFuncCall(name, solver)
        }
    }

    // only called by util
    def traverseBlock(name : String, stats : List[Stat], context: Context) : Unit = {
      for (stat <- stats) {
        stat match { // there will be a lot here
          case bool @ Lit.Boolean(value) =>
            // add assert statement
            utilSolver(name).add(getBoolExpr(bool, context))
          case app @ Term.Apply(Term.Select(Term.Name("util"), Term.Name(name)), args) => // handle params
            // this is function call within util function
            if (utilFuncs(name) != null) {
              addFuncCall(name, utilSolver(name))
            }
          case _  =>
            println("Statement does not exist")
            pause()

        }
      }
    }

    def traverseDefns(defns : List[Stat]) : Unit = {
      for (defn <- defns) {
        defn match {
          case Defn.Def(a, Term.Name(name), b, params, typ, Term.Block(stats)) => // handle params by saving them as variables
            createContext(name)
            createUtilSolver(name)
            traverseBlock(name, stats, utilCtx(name))
          case _ =>
            println("TERM TYPE MISMATCH : Definition")
            null
        }
      }
    }

    def traverseObject(obj : Defn.Object) : Unit = obj match { // doesnt return anything, just builds the map
      case Defn.Object(mods, name, Template(a, b, c, list)) =>
        traverseDefns(list)
      case _ =>
        println("TERM TYPE MISMATCH : Object(2)")
        null
    }

    def traverseUtil(utilTree : Tree):Unit = {
      utilTree match{
        case Source(stats) =>
          for (stat <- stats) {
            stat match {
              case obj @ Defn.Object(mods, name, template) =>
                traverseObject(obj)
            }
          }
        case _ =>
          println("TERM TYPE MISMATCH : Object")
      }

      for (solv <- utilSolver) {
        if(solv._2.check == Status.SATISFIABLE) {
          val model = solv._2.getModel
          println("Model " + solv._1 + " : ")
          println(model.toString)
          utilFuncs = utilFuncs + (solv._1 -> model)
        }
        else {
          println("There is no model, function is unsatisfiable")
          utilFuncs = utilFuncs + (solv._1 -> null)
          // no model assignment if unsat
        }
      }

    } // for getting util specifically

    def traverse(conditionTree : Term):Unit={
      solver.add(getBoolExpr(conditionTree, ctx))
    } // call to traverse normal
  }

  def generateUtilFunctions(util : String) : Unit = {

    // needs to be parsed for each new condition combination
    var utilTree : Tree = null

    if (util.contains("def")) {
      //utilTree = toolbox.parse(util)
      utilTree = util.parse[Source].get
      println(" ++++++++++++++ UTIL TREE ++++++++++++++")
      //println(showRaw(utilTree))
      println(utilTree.structure)
      println(" +++++++++++++++++++++++++++++++++++++++")
      //traverser.traverse(utilTree)
      // now build one whole assert statement based on each function in the util tree
      // no, the types of the functions are declared first and then the functions and their parameters are inputted and asserted
      // nope, change the function called into z3 and use assert statement on returned boolean value (im bad at explaining)

      /*
      utilTree match {
        case Block(block, empty) =>
          block match {
            case List(elts) =>

            case Apply(fun, args) =>
              smtlibVariables = smtlibVariables + "(declare-fun " + fun.toString() + " )\n"
          }
        case _ =>
          println("This is not a block")
      }
       */

      pause()
      traverser.traverseUtil(utilTree)

      for (context <- utilCtx.toIterator) {
        println("Function : " + context._1)
        println(traverser.getUtilSolver(context._1).toString)
        println()
      }

    }
    else {
      utilTree = null
    }

    println("Util traversed")
    pause()
  }

  def generateFormulas(conditions : String, variables : Map[String, String]) : Unit = {
    // use toolbox to parse through command
    // use case matching to traverse the created tree recursively
    // in each case write the corresponding smtlib format command



    for (variable <- variables) {
//      if (variable._2.contains("ean")) {
//        smtlibVariables = smtlibVariables + "(declare-const " + variable._1 + " Bool)\n"
//      }
//      else {
//        smtlibVariables = smtlibVariables + "(declare-const " + variable._1 + " " + variable._2 + ")\n"
//      }
      variable._2 match {
        case "Int" =>
          variablesInt = variablesInt + (variable._1 -> ctx.mkIntConst(variable._1))
        case "Boolean" =>
          variablesBool = variablesBool + (variable._1 -> ctx.mkBoolConst(variable._1))
        case _ =>
      }
    }

//    if (conditions.contains("util")) {
//      //utilTree = toolbox.parse(util)
//      utilTree = util.parse[Source].get
//      println(" ++++++++++++++ UTIL TREE ++++++++++++++")
//      //println(showRaw(utilTree))
//      println(utilTree.structure)
//      println(" +++++++++++++++++++++++++++++++++++++++")
//      //traverser.traverse(utilTree)
//      // now build one whole assert statement based on each function in the util tree
//      // no, the types of the functions are declared first and then the functions and their parameters are inputted and asserted
//      // nope, change the function called into z3 and use assert statement on returned boolean value (im bad at explaining)
//
//      /*
//      utilTree match {
//        case Block(block, empty) =>
//          block match {
//            case List(elts) =>
//
//            case Apply(fun, args) =>
//              smtlibVariables = smtlibVariables + "(declare-fun " + fun.toString() + " )\n"
//          }
//        case _ =>
//          println("This is not a block")
//      }
//       */
//
//    }
//    else {
//      utilTree = null
//    }

    //println("Declared variables ##")
    //println(smtlibVariables)

    //val conditionTree = toolbox.parse(conditions)
    val conditionTree = conditions.parse[Term].get
    println(" ++++++++++++++ CONDITION TREE ++++++++++++++")
    //println(showRaw(conditionTree))
    println(conditionTree.structure)
    println(" ++++++++++++++++++++++++++++++++++++++++++++")
    //var utilTree = conditionTree // temporary value

    pause()

    traverser.traverse(conditionTree)

    println("Traversed")
    pause()

//    var smtlibConditions = "(set-logic QF_LIA)\n" + smtlibVariables + traverser.getSmtlibString + "(check-sat)\n(get-model)"

//
//    println("--- SMTLIB ---")
//    println(smtlibConditions)
//    println("--------------")


//    smtlibConditions =
//      """
//        |(declare-const x Int)
//        |(declare-const y Int)
//        |(declare-const z Int)
//        |(declare-const a1 (Array Int Int))
//        |(declare-const a2 (Array Int Int))
//        |(declare-const a3 (Array Int Int))
//        |(assert (= (select a1 x) x))
//        |(assert (= (store a1 x y) a1))
//        |(assert (not (= x y)))
//        |(check-sat)
//        |""".stripMargin

//    smtlibConditions

  }

  def convertConditionsToSMTLIB(smtlibConditions : String) : Stream[Command] = { // change conditions to list to separate conditions of different labels


    // I CANT USE TERM

    //var smtlibConditions = generateSMTLIBString(conditions, variables, util)
    //println("``` SMTLIB COMMANDS ```")
    //println(smtlibConditions)

    val is = new java.io.StringReader(smtlibConditions)
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)

    println("Reading script...")

    val script: List[Command] = {
      var cmds = new ListBuffer[Command]
      var cmd : Command = parser.parseCommand //SetOption(ProduceModels(true))  //parser.parseCommand
      print("Command: " + cmd)
      while(cmd != null) {
        cmds.append(cmd)
        cmd = parser.parseCommand
        print("Command: " + cmd)
      }
      cmds.toList
    }

    script.toStream

  }

//  def propositionalVariables(formula : Term) : Set[String] = formula match {
//    case True() | False() => Set() // syntax for defining multiple cases at once
//    case QualifiedIdentifier(SimpleIdentifier(id), _) => Set(id.name) // propositional variable with SSymbol identifier "id"
//    case Not(f) => propositionalVariables(f)
//    case Or(disjuncts@_*) => // The Or and And constructors take variable-length argument sequences; this is the generalised pattern-matching sequence for matching an Or with "disjuncts" as the sequence of disjunct terms (which is a Scala Seq[Term] value)
//      disjuncts.foldLeft[Set[String]](Set())((set,d) => set union propositionalVariables(d)) // See foldLeft in http://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
//    case And(conjuncts@_*) => conjuncts.foldLeft[Set[String]](Set())((set,c) => set union propositionalVariables(c))
//    case Implies(f,g) => propositionalVariables(f) union propositionalVariables(g)
//    case Equals(f,g) => propositionalVariables(f) union propositionalVariables(g)
//  }
//
//  private val bool = Sort(Identifier(SSymbol("Bool"), Seq()))
//
//  private def declareVariable(name: String) = {
//    DeclareFun(SSymbol(name), Seq(), bool)
//  }
//
//  private def declareVariables(formula: Term) = {
//    propositionalVariables(formula) map declareVariable
//  }

//  def processConditions(commandStream : Stream[Command]) : Stream[Command] = { // ParserTerms
//    // do the things to read the smtlib file and change them to term NO TO COMMANDS
//    // OR
//    // there s an smtlib command that makes z3 check sat (see z3 easy tutorial) <-- TRY THIS FIRST
//    // ^ this actually doesn't work nevermind
//
//
//    val script = convertConditionsToSMTLIB(conditions, variables, util)
//
//    script
//  }
  // COMBINE THE ABOVE AND BELOW TWO FUNCTIONS
//  private def createInputStream(formula: Term): Stream[Command] = {
//    // find a method to convert the conditions into smtlib format easily
//
//    SetOption(ProduceModels(true)) #::
//      SetLogic(QF_UF()) #::
//      declareVariables(formula).toStream #:::
//      Assert(formula) #::
//      CheckSat() #::
//      //    GetModel() #::
//      Stream.empty[Command]

//    SetOption(ProduceModels(true)) #::
//      SetLogic(QF_UF()) #::
//      //declareVariables(formula).toStream #::// declareVariables(formula).toStream #::: // dont do this stuff just convert the conditions to commands and it will read those
//      Assert(formula) #::
//      CheckSat() #::
//      //    GetModel() #::
//      Stream.empty[Command]
//  }


  def processInput(commandStream : Stream[Command]): OutputStream => Unit = {
    (stdin: OutputStream) => {
      val printer = new PrintWriter(new BufferedOutputStream(stdin))
      commandStream.foreach(
        cmd => {
          printer.write(cmd.toString())
        }
      )
      printer.close()
    }
  }

  // used for conditions
  def checkUnsat() : Boolean = { // change this to accept formula
    // return nothing if satisfiable
    // save current condition
    // for each combination of previous conditions
    //    check satisfiability with last condition
    //    return if the unsat part found, dont continue loop to find more complex combinations
    // start with last cond
    //    and 1 other cond
    // keep adding combinations from there

    // SOMEHOW MAKE THIS STUFF BELOW WORK LIKE STUFF ABOVE
    // save aggregated condition as a list instead of a string so that they can be handled separately
    //val formula = True() // processConditions(aggConds)
    //val

    println("Checking the following ->")
    println(traverser.getSolver.toString)
    if(traverser.getSolver.check == Status.SATISFIABLE) {
      println("\nSAT ~~~~~~~~~~~~~~~~~~~\n")
      traverser.clearSolver()
      true
    }
    else {
      println("\nUNSAT ~~~~~~~~~~~~~~~~~~~\n")
      traverser.clearSolver()
      false
    }


//    val process = Process(z3_path, Seq("-smt2", "-in"))
//    var result: String = null
//    val processIO = new ProcessIO(
//      outputStream,
//      stdout => {
//        val reader = new BufferedReader(new InputStreamReader(stdout))
//        result = reader.readLine()
//      },
//      stderr => {
//        scala.io.Source.fromInputStream(stderr)
//          .getLines.foreach(println)
//      }
//    )
//    val handle = process.run(processIO)
//    assert(handle.exitValue() == 0)
//    assert(result == "sat" || result == "unsat")
//    if (result == "sat") {
//      println("\nSAT ~~~~~~~~~~~~~~~~~~~\n") // Some(Map())
//      true
//    } else {
//      println("\nUNSAT ~~~~~~~~~~~~~~~~~~\n") // None
//      false
//    }


    //null
  }

  // used for util
  def checkSat() : Model  = {
    if (traverser.getSolver.check == Status.SATISFIABLE){
      traverser.getSolver.getModel
    }
    else {
      null
    }
  }

}
