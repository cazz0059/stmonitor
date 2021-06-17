package monitor.parser

//import smtlib.trees.Commands._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import com.microsoft.z3._

import scala.meta.contrib.XtensionTreeEquality

//import java.io._

//import scala.reflect.runtime._
//import scala.reflect.runtime.universe._
//import scala.tools.reflect.ToolBox

import scala.meta._
import scala.language.existentials

//import scala.sys.process._

//import z3

class STSolverZ3 {

  //  private lazy val z3_path = {
  //    sys.env.getOrElse("Z3_EXE", "z3.exe")
  //  }

  //private val toolbox = currentMirror.mkToolBox()

  // variables in traverser are reset each time, and globals arent?

  var solvable = true // turns to false when a function is unsolvable

  // enumeration
  val integer = 0
  val boolean = 1
  val string = 2

  class Variables {
    var vars : Map[String, (Expr[_], Int)] = Map()
    //    var integers : Map[String, IntExpr] = Map()
    //    var booleans : Map[String, BoolExpr] = Map()
    //    var strings : Map[String, Expr[SeqSort[BitVecSort]]] = Map()
  }

  private val ctx: Context = new Context(new java.util.HashMap[String, String])

  //private var smtlibVariables = ""
  //  private var variablesInt : Map[String, IntExpr] = Map()
  //  private var variablesBool : Map[String, BoolExpr] = Map()
  //  private var variablesString : Map[String, Expr[SeqSort[BitVecSort]]] = Map()
  private var variables : Variables = new Variables()

  //private var utilParams : Map[String, SortedMap[String, (Expr[_], Int)]] = Map() // Map[String, Variables]
  //private var utilParams : SortedMap[String, (Expr[_], Int)] = SortedMap() // Map[String, Variables]
  private var utilFuncs : Map[String, (List[Term.Param], List[Stat])] = Map() // params, stats in block // Defn.Def(a, Term.Name(name), b, List(params), typ, Term.Block(stats))
  //private var utilCtx : Map[String, Context] = Map()

  def aggregate(expressionList : ListBuffer[BoolExpr]) : BoolExpr = {
    if(expressionList.tail.nonEmpty)
      ctx.mkAnd(expressionList.head.asInstanceOf[Expr[BoolSort]], aggregate(expressionList.tail))
    else
      expressionList.head
  }

  def addVars(typ : Int, variable : String) : Unit = {
    if(typ == integer) {
      variables.vars = variables.vars + (variable -> (ctx.mkIntConst(variable), integer))
    }
    else if(typ == boolean) {
      variables.vars = variables.vars + (variable -> (ctx.mkBoolConst(variable), boolean))
    }
    else if(typ == string) {
      variables.vars = variables.vars + (variable -> (ctx.mkConst(ctx.mkSymbol(variable), ctx.mkStringSort()), string))
    }
  }

  def addParams(utilPms : mutable.LinkedHashMap[String, (Expr[_], Int)], typ : Int, variable : String) : mutable.LinkedHashMap[String, (Expr[_], Int)] = { // name : String,
    var utilParams = utilPms
    if(typ == integer) {
      //val newParam : SortedMap[String, (Expr[_], Int)] = SortedMap(variable -> (ctx.mkIntConst(variable), typ))
      val newParam : (String, (Expr[_], Int)) = variable -> (ctx.mkIntConst(variable), typ)
      //val newParams : SortedMap[String, (Expr[_], Int)] = utilParams(name) ++ newParam
      utilParams += newParam// .integers = utilParams(name).integers + (variable -> ctx.mkIntConst(variable))
    }
    else if(typ == boolean) {
      val newParam : (String, (Expr[_], Int)) = variable -> (ctx.mkBoolConst(variable), typ)
      //val newParams : SortedMap[String, (Expr[_], Int)] = utilParams(name) ++ newParam
      utilParams += newParam // (name -> newParams)
    }
    else if(typ == string) {
      val newParam : (String, (Expr[_], Int)) = variable -> (ctx.mkConst(ctx.mkSymbol(variable), ctx.mkStringSort()), typ)
      //val newParams : SortedMap[String, (Expr[_], Int)] = utilParams(name) ++ newParam
      //utilParams = utilParams + (utilParams(name) ++ (variable -> ctx.mkConst(ctx.mkSymbol(variable), ctx.mkStringSort())))
      utilParams += newParam //(name -> newParams)
    }
    else
      null
  }

  def addFuncVars(funcVars : Variables, typ : Int, variable : String) : Variables = {
    if(typ == integer) {
      funcVars.vars = funcVars.vars + (variable -> (ctx.mkIntConst(variable), integer))
    }
    else if(typ == boolean) {
      funcVars.vars = funcVars.vars + (variable -> (ctx.mkBoolConst(variable), boolean))
    }
    else if(typ == string) {
      funcVars.vars = funcVars.vars + (variable -> (ctx.mkConst(ctx.mkSymbol(variable), ctx.mkStringSort()), string))
    }
    funcVars
  }

  def removeFuncVars(funcVars : Variables, variable : String) : Variables = {
    val funcVarsTemp = funcVars
    funcVarsTemp.vars = funcVarsTemp.vars - variable
    funcVarsTemp
  }


  private var assertions : Map[String, BoolExpr] = Map()
  // find way to eliminate conditions that have nothing to do with the unsatisfiability - saveUnsatConds()
  private var lemmas : List[Set[Term]] = List() // SMT read this and see if it is the same semantics
  //private val cnfTransformer = new TransformToCnf()

//  def pause() : Unit = {
//    print("Pausing...")
//    scala.io.StdIn.readLine()
//  }

  def addAssertionLabel(label : String, assertion : BoolExpr) : Unit = {
    assertions = assertions + (label -> assertion)
  }

  def getLemmas : List[Set[Term]] = {
    lemmas
  }

  // change to list of strings?
  def addLemma(unsatCond : Set[Term]) : Unit = {
//    println("Adding lemma...")
    lemmas = unsatCond :: lemmas
  }

  def compareToLemmas(checkCond : Set[Term]) : Boolean = {
    // can be done with toolbox - no use cnf instead, I think it saves easier

    // get semantics of aggConds - cnf clauses
    // for each lemma
    //    get semantics of lemma
    //    compare semantics - check if clauses in lemma make up subset of clauses in aggConds
    for(lemma <- lemmas) {

      // change the trees to string and compare them then
//      var stringCond : Set[String] = Set()
//      for (cond <- checkCond) stringCond = stringCond ++ cond
//      var stringLemma : Set[String] = Set()
//      for (lma <- lemma) stringLemma = stringLemma ++ lma

      var equCheck = true // and with this - checks that all elts in set are equal
      for(lm <- lemma){
        var currLemmaCheck = false
        for(cond <- checkCond){
          if(lm.isEqual(cond)) {
            currLemmaCheck = true
          }
        }
        equCheck = equCheck && currLemmaCheck
      }

      if(equCheck) { // stringCond.equals(stringLemma)
        return false
      }
    }
    true
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
    private var global = true
    private var isFromCond = false


    def getSolver: com.microsoft.z3.Solver = {
      solver
    }
    def clearSolver() : Unit = {
      solver = ctx.mkSolver()
      count = 0
    }

    def getVars(name : String, typ : Int) : Expr[_] = {
      variables.vars(name)._1
    }


    def getFuncVars(funcVars : Variables, name : String) : (Expr[_], Int) = { // , typ : Int

      // check keysets
      // first check when no number
      // then see current count, and go down from there, until variable found

      val keyset = funcVars.vars.keySet

      if(keyset.contains(name))
        funcVars.vars(name)

      else {
        var tempCount = count
        while(tempCount >= 0) {
          val currName = name + "_" + tempCount
          if (keyset.contains(currName))
            return funcVars.vars(currName)
          tempCount = tempCount - 1
        }
        println("Value does not exist in function variables")
        null
      }

    }

    private var count = 0
    def incCount() : Unit = {
      count+=1
    }

    private var temp_count = 0

    // these return all possible combinations of a desired type
    def getBoolExpr(arg : Term, funcVars : Variables) : BoolExpr = arg match {
      case Lit.Boolean(value) =>
        if(value)
          ctx.mkTrue()
        else
          ctx.mkFalse()
        // cannot have this constant param name, there may be others in the same function
//        val temp_bool_const = ctx.mkBoolConst("temp_traverse_bool_const_" + count)
//        incCount()
//        var temp_and : BoolExpr = null
//
//        if(!value) temp_and = ctx.mkAnd(temp_bool_const, ctx.mkNot(temp_bool_const))
//        else temp_and = ctx.mkAnd(temp_bool_const, temp_bool_const) // temp_bool_const // why did i set it to null? because if it is itself it is always sat

//        temp_and


      case termName @ Term.Name(name) =>
        traverseBoolVar(termName, funcVars)

      case infix @ Term.ApplyInfix(a, b, c, d) => traverseBoolInfix(infix, funcVars)

      case unary @ Term.ApplyUnary(o, a) => traverseBoolUnary(unary, funcVars)

      case funcCall @ Term.Apply(fun, args) =>
        var funcBool = traverseFuncBool(funcCall, funcVars)
        if(funcBool == null) solvable = false
        funcBool
      //null

      case _ =>
        solvable = false
        null
    }

    // call getArithExpr as well
    // update like the one above
    def getIntExpr(arg : Term, funcVars : Variables) : ArithExpr[_] = arg match {

      case Lit.Int(value) =>
        val intVal : ArithExpr[_] = ctx.mkInt(value)
        intVal

      case term @ Term.Name(name) => traverseIntVar(term, funcVars)

      case term @ Term.ApplyInfix(a, b, c, d) =>
        //val arithExpr : IntExpr = getArithExpr(term, context, funcVars).asInstanceOf
        val arithExpr = getArithExpr(term, funcVars)
        arithExpr

      case Term.ApplyUnary(o, a) =>
        val arithExpr = traverseIntUnary(Term.ApplyUnary(o, a), funcVars)
        arithExpr

      case _ =>
        solvable = false
        null
    }

    def getArithExpr(arg : Term, funcVars : Variables) : ArithExpr[_] = arg match {
      case Term.ApplyInfix(a, b, c, d) => traverseArithInfix(Term.ApplyInfix(a, b, c, d), funcVars)
      case _ =>
        solvable = false
        null
    }

    def getStringExpr(arg : Term, funcVars : Variables) : Expr[SeqSort[BitVecSort]] = arg match {
      case term @ Term.Name(name) => traverseStringVar(term, funcVars)
      case Lit.String(value) =>
        val stringVal : Expr[SeqSort[BitVecSort]] = ctx.mkString(value)
        stringVal
      case _ =>
        solvable = false
        null
    }

    def getExpr(arg : Term, funcVars : Variables) : (Expr[_], Int) = {

      var expr : Expr[_] = getBoolExpr(arg, funcVars)
      var typ = boolean
      if (expr == null) {
        expr = getIntExpr(arg, funcVars)
        typ = integer
      }
      //      if (expr == null) {
      //        expr = getArithExpr(arg, context, funcVars)
      //        typ = integer
      //      }
      if (expr == null) {
        expr = getStringExpr(arg, funcVars)
        typ = string
      }
      if (expr == null) {
        println("ERROR : EXPR NOT FOUND, Solvable? " + solvable)
        //solvable = false
        return (null, -1)
      }
      solvable = true
      (expr, typ)
    }


    // called from traverse func call, which is called by get bool expr


    // remember that embedded operations exist, so i cant just put "assert" in front of all the operators
    //    try and build the conditions (select and apply) recursively before adding the assert
    // use smt-lib List (recursive) some combination of that to unfold a recursive function???
    // or Tree??
    // z3 cannot prove by induction, so unfolding needs to take place to prove by deduction
    def traverseBoolVar(term : Term.Name, funcVars : Variables) : BoolExpr = term match {
      // probably is a singular boolean variable, as in any other case it would form part of a formula
      case Term.Name(name) =>
        if(global || isFromCond) {
          if (variables.vars.keySet.contains(name) && variables.vars(name)._2 == boolean) { // .integers // isInstanceOf[BoolExpr]
            variables.vars(name)._1.asInstanceOf[BoolExpr]
          } else { // search also in parameters and in decls within function
            // see how to differentiate between all the variables' names if they are the same and all
            null
          }
        }
        else {
          val boolVar = getFuncVars(funcVars, name)
          if (boolVar != null && boolVar._2 == boolean) { // .integers // funcVars.vars.keySet.contains(name) && funcVars.vars(name)._2 == boolean
            boolVar._1.asInstanceOf[BoolExpr]//funcVars.vars(name).asInstanceOf[BoolExpr]
          } else {
            null
          }
        }
      //null //else

      //solver.add(variablesBool(name))
      case _ =>
        null
    }

    // make a global flag to indicate if global or within function call
    // if from function, build a new set of vars
    // add parameter decls to said set
    // decl params within push
    // add also any var decls within function
    // pass the set for the corresponding function (dont make this var global, pass is because if function call within function we need that var again)
    // rip memory
    // make flag global, its fine since i can keep track of when in function or not based on position in sequence
    // when global (conditions), just save variable param to null
    // add methods for push and pop of function call, and add setting global to true or false
    // wait only change to false when make sure to return in conditions, not when function call over - very important
    def traverseIntVar(term : Term.Name, funcVars : Variables) : IntExpr = term match {
      // probably is a singular boolean variable, as in any other case it would form part of a formula
      case Term.Name(name) =>

        if(global || isFromCond) {
          if (variables.vars.keySet.contains(name) && variables.vars(name)._2 == integer) { // .integers
            variables.vars(name)._1.asInstanceOf[IntExpr]
          } else { // search also in parameters and in decls within function
            // see how to differentiate between all the variables' names if they are the same and all
            null
          }
        }
        else { // only come here is called from util function
          val intVar = getFuncVars(funcVars, name)
          if (intVar != null && intVar._2 == integer) { // .integers
            intVar._1.asInstanceOf[IntExpr]
          } else {
            null
          }
        }
      //        if (variables.vars.keySet.contains(name)) // .integers
      //          variables.vars(name).asInstanceOf[IntExpr]
      //        else { // search also in parameters and in decls within function
      //          // see how to differentiate between all the variables' names if they are the same and all
      //          null
      //        }

      //solver.add(variablesBool(name))
      case _ =>
        null
    }

    def traverseStringVar(term : Term.Name, funcVars : Variables) : Expr[SeqSort[BitVecSort]] = term match {
      // probably is a singular boolean variable, as in any other case it would form part of a formula
      case Term.Name(name) =>
        if(global || isFromCond) {
          if (variables.vars.keySet.contains(name) && variables.vars(name)._2 == string) { // .integers
            variables.vars(name)._1.asInstanceOf[Expr[SeqSort[BitVecSort]]]
          } else { // search also in parameters and in decls within function
            // see how to differentiate between all the variables' names if they are the same and all
            null
          }
        }
        else {
          val stringVar = getFuncVars(funcVars, name)
          if (stringVar != null && stringVar._2 == string) { // .integers
            stringVar._1.asInstanceOf[Expr[SeqSort[BitVecSort]]]
          } else {
            null
          }
        }
      //solver.add(variablesBool(name))

      case _ =>
        null
    }

    def traverseBoolInfix(term : Term.ApplyInfix, funcVars : Variables) : BoolExpr = term match {
      case Term.ApplyInfix(lhs, Term.Name("&&"), tArgs, args) =>
        var lhsBool = getBoolExpr(lhs, funcVars)
        if(lhsBool != null) {
          for (arg <- args) {
            val rhsBool = getBoolExpr(arg, funcVars)
            //println(lhsBool + " && " + rhsBool)
            if (rhsBool != null)
              lhsBool = ctx.mkAnd(lhsBool, rhsBool)
          }
        }
        lhsBool

      case Term.ApplyInfix(lhs, Term.Name("||"), tArgs, args) =>
        var lhsBool = getBoolExpr(lhs, funcVars)
        if(lhsBool != null) {
          for (arg <- args) {
            val rhsBool = getBoolExpr(arg, funcVars)
            if (rhsBool != null)
              lhsBool = ctx.mkOr(lhsBool, rhsBool)
          }
        }
        lhsBool

      case Term.ApplyInfix(lhs, Term.Name("<"), tArgs, args) =>
        val lhsInt = getIntExpr(lhs, funcVars)
        val rhsInt = getIntExpr(args.head, funcVars)
        var lt : BoolExpr = null
        if((lhsInt != null) && (rhsInt != null))
          lt = ctx.mkLt(lhsInt.asInstanceOf[Expr[ArithSort]], rhsInt.asInstanceOf[Expr[ArithSort]])
        lt

      case Term.ApplyInfix(lhs, Term.Name(">"), tArgs, args) =>
        val lhsInt = getIntExpr(lhs, funcVars)
        val rhsInt = getIntExpr(args.head, funcVars)
        var gt : BoolExpr = null
        if((lhsInt != null) && (rhsInt != null))
          gt = ctx.mkGt(lhsInt.asInstanceOf[Expr[ArithSort]], rhsInt.asInstanceOf[Expr[ArithSort]])
        gt

      case Term.ApplyInfix(lhs, Term.Name("<="), tArgs, args) =>
        val lhsInt = getIntExpr(lhs, funcVars)
        val rhsInt = getIntExpr(args.head, funcVars)
        var lte : BoolExpr = null
        if((lhsInt != null) && (rhsInt != null))
          lte = ctx.mkLe(lhsInt.asInstanceOf[Expr[ArithSort]], rhsInt.asInstanceOf[Expr[ArithSort]])
        lte

      case Term.ApplyInfix(lhs, Term.Name(">="), tArgs, args) =>
        val lhsInt = getIntExpr(lhs, funcVars)
        val rhsInt = getIntExpr(args.head, funcVars)
        var gte : BoolExpr = null
        if((lhsInt != null) && (rhsInt != null))
          gte = ctx.mkGe(lhsInt.asInstanceOf[Expr[ArithSort]], rhsInt.asInstanceOf[Expr[ArithSort]])
        gte

      case Term.ApplyInfix(arg, Term.Name("=="), tArgs, args) =>
        var lhs : Expr[_] = getIntExpr(arg, funcVars)
        var rhs : Expr[_] = null
        if(lhs != null) {
          rhs = getIntExpr(args.head, funcVars)
        }
        else {
          lhs = getBoolExpr(args.head, funcVars)
          rhs = getBoolExpr(args.head, funcVars)
        }

        var eq : BoolExpr = null
        if((lhs != null) && (rhs != null))
          eq = ctx.mkEq(lhs, rhs)
        eq

      case Term.ApplyInfix(arg, Term.Name("!="), tArgs, args) =>
        var lhs : Expr[_] = getIntExpr(arg, funcVars)
        var rhs : Expr[_] = null
        if(lhs != null) {
          rhs = getIntExpr(args.head, funcVars)
        }
        else {
          lhs = getBoolExpr(args.head, funcVars)
          rhs = getBoolExpr(args.head, funcVars)
        }

        var neq : BoolExpr = null
        if((lhs != null) && (rhs != null))
          neq = ctx.mkNot(ctx.mkEq(lhs, rhs))
        neq

      case _ =>
        null
    }

    def traverseArithInfix(term : Term.ApplyInfix, funcVars : Variables) : ArithExpr[_] = term match {
      case Term.ApplyInfix(lhs, Term.Name("+"), tArgs, args) =>
        var lhsInt : ArithExpr[_] = ctx.mkAdd(getIntExpr(lhs, funcVars)) // ctx.mkIntConst("0")
        for (arg <- args){
          val rhsInt = getIntExpr(arg, funcVars)
          if ((lhsInt != null) && (rhsInt != null))
            lhsInt = ctx.mkAdd(lhsInt, rhsInt)
        }
        lhsInt

      case Term.ApplyInfix(lhs, Term.Name("-"), tArgs, args) =>
        var lhsInt : ArithExpr[_] = ctx.mkAdd(getIntExpr(lhs, funcVars))
        for (arg <- args) {
          val rhsInt = getIntExpr(arg, funcVars)
          if ((lhsInt != null) && (rhsInt != null))
            lhsInt = ctx.mkSub(lhsInt, rhsInt)
        }
        lhsInt

      case Term.ApplyInfix(lhs, Term.Name("*"), tArgs, args) =>
        var lhsInt : ArithExpr[_] = ctx.mkAdd(getIntExpr(lhs, funcVars))
        for (arg <- args) {
          val rhsInt = getIntExpr(arg, funcVars)
          if ((lhsInt != null) && (rhsInt != null))
            lhsInt = ctx.mkMul(lhsInt, rhsInt)
        }
        lhsInt

      case Term.ApplyInfix(lhs, Term.Name("/"), tArgs, args) =>
        var lhsInt : ArithExpr[_] = ctx.mkAdd(getIntExpr(lhs, funcVars))
        for (arg <- args) {
          val rhsInt = getIntExpr(arg, funcVars)
          if ((lhsInt != null) && (rhsInt != null))
            lhsInt = ctx.mkDiv(lhsInt, rhsInt)
        }
        lhsInt

      case _ =>
        null
    }

    def traverseBoolUnary(term : Term.ApplyUnary, funcVars : Variables) : BoolExpr = term match {
      case Term.ApplyUnary(null, arg) =>
        getBoolExpr(arg, funcVars)
      case Term.ApplyUnary(op, arg) =>
        val unExp = getBoolExpr(arg, funcVars)
        if(unExp != null)
          ctx.mkNot(unExp)
        else null
      case _ =>
        null
    }

    def traverseIntUnary(term : Term.ApplyUnary, funcVars : Variables) : ArithExpr[_] = term match {
      case Term.ApplyUnary(null, arg) =>
        getIntExpr(arg, funcVars)
      case Term.ApplyUnary(op, arg) =>
        val unExp = getIntExpr(arg, funcVars)
        if (unExp != null){
          // ctx.mkSub(getIntExpr(Lit.Int(0), funcVars), unExp)
          ctx.mkUnaryMinus(unExp)
        }
        else
          null // ctx.mkBVNeg(ctx.mkInt2BV(getIntExpr(arg)))
      case _ =>
        null
    }

    ///--------------------------------

    // called only from conditions, not util file
    // gets list of all interpretations in the function call as a big AND of assertions in BoolExpr format, called from getBoolExpr to be used anywhere

    // in function call make sure to set global and fromCond
    // dont add anything, just keep adding to context
    def traverseFuncBool(app : Term.Apply, funcVars : Variables) : BoolExpr = app match { // does the adding itself
      case Term.Apply(Term.Select(Term.Name("util"), Term.Name(name)), args) =>
        // this is function call within util function
        if (utilFuncs.keySet.contains(name)) {
          global = false
          isFromCond = true
          val funcExpr : BoolExpr = traverseBlock(name, args, funcVars)//, solver)
          global = true
          funcExpr
        }
        else {
          println("Function does not exist")
          //pause()
          null
        }
      case _  =>
        println("Function invalid")
        //pause()
        null
    }
    ///-----------------------------------


    // ALL STATEMENTS HERE ARE BASIC, limitations include:
    //   no arrays ADD ARRAYS
    //   no scala functions
    //   no for loops
    //   no if-then-else
    //   variable naming conventions to avoid nondeterminism/reassignments/confusion in assertions
    //       try and name variables in functions and params different from global as much as possible
    //       if naming them the same make sure the parameters match the arguments and not get mixed up
    // make something that if it cant parse it just return unknown
    // dont break the whole system if the util function cannot be parsed
    // so still parse the util functions in the beginning, just skip branches that have util functions because we dont know how to parse them
    // dont parse util functions in the beginning (while loop needs previous conditions), just parse when accessed.

    // REASONS WHY NOT OPTIMISED
    // doe snot support recursion
    // integers cannot be interpreted as range
    // while loops need previous conditions, or else almost always sat

    // traversed and solver immediately on call
    // everytime this is called, solvable is chcked
    def traverseStat(stat : Stat, fVars : Variables) : (ListBuffer[BoolExpr], Variables) = {
      var funcVars = fVars
      var expressions : ListBuffer[BoolExpr] = ListBuffer()
      stat match{
        case bool @ Lit.Boolean(value) =>
          // add assert statement
          // solver.add(getBoolExpr(bool, null)) // utilSolver(name) // funcVars is null wont make a difference because its just a boolean literal
          var boolExpr = getBoolExpr(bool, null)
          if(boolExpr != null) expressions += boolExpr // getBoolExpr(bool, null)
        case Defn.Val(mods, List(Pat.Var(Term.Name(name))), decltpe, rhs) =>
          val (getRHS, typTemp) = getExpr(rhs, funcVars)
          if(!solvable) return (ListBuffer(), null)
          //val getRHS : Sort = getRHSTemp.getSort.asInstanceOf[Sort] // : Sort
          // fix this
          // wait adding things to context doesnt change stuff
          // like adding multiple adds and not to the same expression
          // now just add the assert equals
          // z3 cannot assign no

          // the assertions of the rhs are already being asserted with the context, maybe this should be changed so its just an assignment rather than an assertion
          //context.mkConst(name, getRHS)
          funcVars = addFuncVars(funcVars, typTemp, name) // adding the variable to the list (not the rhs, just the name and a const with it)
          expressions += ctx.mkEq(getFuncVars(funcVars, name)._1, getRHS) // "assigning" the rhs to the created (above) constant

          // handle arrays
          // support only array declaration and element access for now
          // change term.name functions to access array elements
          // make variables
          // hold on a minute the solver cant accept constant assignments
          // nvm, just call Defn.Var on each position

          //pause()
        case Defn.Var(mods, List(Pat.Var(Term.Name(name))), decltpe, rhs) =>

          val (getRHS, typTemp) = getExpr(rhs.get, funcVars)
          if(!solvable) return (ListBuffer(), null)
          //val getRHS : Sort = getRHSTemp.getSort.asInstanceOf[Sort] // : Sort
          //context.mkConst(name, getRHS)
          funcVars = addFuncVars(funcVars, typTemp, name)
          expressions += ctx.mkEq(getFuncVars(funcVars, name)._1, getRHS)
          //pause()
        case Term.Assign(Term.Name(lhsName), rhs) =>
          // RESTRICT NAMING CONDITIONS TO AS MUCH AS POSSIBLE NOT USE "_n"

          // only func vars can be changed
          // just change the name of the fuc vars
          // give them numbers for every assignment
          //var lhsExpr = getExpr(lhs, funcVars)
          val lhsExpr = getFuncVars(funcVars, lhsName)
          val rhsExpr = getExpr(rhs, funcVars)._1
          val lhsNewName = lhsName + "_" + count; incCount()
          funcVars = addFuncVars(funcVars, lhsExpr._2, lhsNewName)
          if(!solvable) return (ListBuffer(), null)
          // assert equals
          val lhsNewExpr = getFuncVars(funcVars, lhsNewName)._1
          expressions += ctx.mkEq(lhsNewExpr, rhsExpr)
          // change name
          //funcVars = removeFuncVars(funcVars, lhsName)

        case Term.ApplyInfix(lhs, op, targs, args) =>
          //solver.add(getBoolExpr(Term.ApplyInfix(lhs, op, targs, args), funcVars))
          var boolExpr = getBoolExpr(Term.ApplyInfix(lhs, op, targs, args), funcVars)
          if (boolExpr != null) expressions += boolExpr
        case Term.If(condTerm, ifBlock, elseBlock) =>
//          solver.push()
//          val cond = getBoolExpr(condTerm, funcVars)
//          solver.add(cond)
//          val ifCheck = solver.check()
//          var condNeg = ctx.mkNot(cond)
//          solver.add(condNeg)
//          val elseCheck = solver.check()
//          solver.pop()
          // no need to do all the hassle of pushing and popping
          // if one branch is unsat, there s always the other
          // so check if the if branch is sat
          // if it is accessible and sat, then done
          // if accessible and unsat, check else block
          //    if else block accessible and sat, then all is sat
          //    if else block accessible and unsat, then all unsat
          //    if else block unaccessible, then all unsat
          // if not accessible, check else block
          //    if else block sat, all is sat
          //    if else block unsat, all is unsat
//          if(ifCheck == Status.SATISFIABLE) { // accessible
//            var ifExpr : Expr[_] = null
//            var elseExpr : Expr[_] = null
//            ifBlock match {
//              case Term.Block(stats) =>
//                for(stat <- stats) {
//                  // (ifExpr, funcVars) = traverseStat(stat, funcVars)
//                  val traversedStat = traverseStat(stat, funcVars)
//                }
//              case _ =>
//                funcVars = traverseStat(ifBlock, funcVars)
//            }
//            // just do normal obvious false to be unsat
//            if(solver.check() == Status.UNSATISFIABLE) { // accessible and unsat
//              if(elseCheck == Status.SATISFIABLE) { // else block accessible
//                elseBlock match {
//                  case Term.Block(stats) =>
//                    for(stat <- stats) funcVars = traverseStat(stat, funcVars)
//                  case _ =>
//                    funcVars = traverseStat(elseBlock, funcVars)
//                }
//                if(solver.check() == Status.UNSATISFIABLE) // accessible and unsat else block
//                  funcVars = traverseStat(Lit.Boolean(false), funcVars) // all unsat
//              }
//              else // inaccessible else block
//                funcVars = traverseStat(Lit.Boolean(false), funcVars) // all unsat
//
//            }
//            else { // not accessible
//              elseBlock match {
//                case Term.Block(stats) =>
//                  for(stat <- stats) funcVars = traverseStat(stat, funcVars)
//                case _ =>
//                  funcVars = traverseStat(elseBlock, funcVars)
//              }
//              if(solver.check() == Status.UNSATISFIABLE) // accessible and unsat else block
//                funcVars = traverseStat(Lit.Boolean(false), funcVars) // all unsat
//            }
//
//          }

          // if if cond sat then add all if block
          // if at the end of the function or conditions it ends up being unsat, we have to check the else block
          //var n = 1
          val cond : BoolExpr = getBoolExpr(condTerm, funcVars)
          temp_count = count // saving count before if block
          val (ifExpr, funcVarsTempIf) = traverseStat(ifBlock, funcVars)
          count = temp_count // resetting count to before if block
          val (elseExpr, funcVarsTempElse) = traverseStat(elseBlock, funcVars)
          // now funcVars should be the same
          //  OR the func vars
          // if the funcvars are the same size (cant have the same keyset because the counter changes)
          if(funcVarsTempIf.vars.keySet == funcVarsTempElse.vars.keySet) { // very buggy
            //expressions += ctx.mkITE(cond, aggregate(ifExpr), aggregate(elseExpr)).asInstanceOf[BoolExpr]
            val ifFullExpr = ctx.mkAnd(cond, aggregate(ifExpr))
            val elseFullExpr = ctx.mkAnd(ctx.mkNot(cond), aggregate(elseExpr))
            val iteFullExpr = ctx.mkOr(ifFullExpr, elseFullExpr)
//            println("If statement : " + iteFullExpr)
            expressions += iteFullExpr
            funcVars = funcVarsTempElse // makes no difference which
          }
          else {
            println("DISCLAIMER")
            println("The if block and else block have different assignments. This format is currently not supported. ")
            println("Statement: " + stat)
            // we cant assume it is satisfiable, we must consider the whole condition as satisfiable
            println("Please make sure that both blocks modify the same variables the same amount of times. ")
            count = temp_count // nothing happened in if, so reset count to before if
          }


          // consider just going through both funcvars and for each variables taht are different, get the last one for each and or them
          // doe snot guarantee unsat detected but better leave in an unsat than delete a sat accidentally

          //if make limitation on if-else that the blocks have to have the same number of assignments for the same variables (very simple if-else blocks) then it is doable
          // as the funcVars will be identical
          // but blocks will change the stuff

        case Term.While(cond, Term.Block(stats)) =>
          // must unroll, no blocks and pushes and pops
          // must change all the decls (as per example)
          //          var count = 0
          //        //{
          //          var funcVarsTemp: Variables = new Variables()
          //          for (funcVar <- funcVars.vars) { // new set of variables
          //            val funcVarNameTemp = funcVar._1 + "_" + count
          //            val funcVarValTemp = funcVar._2
          //            funcVarsTemp.vars = funcVarsTemp.vars + (funcVarNameTemp -> funcVarValTemp)
          //            count = count + 1
          //          }
          //          var loopCond = getBoolExpr(expr, funcVars)
          //          body match {
          //            case Term.Block(stats) =>
          //              var block : ListBuffer[Expr[_]] = ListBuffer()
          //              for(bodyStat <- stats) {
          //                //block = block + traverseStat(bodyStat, funcVarsTemp)
          //              }
          //          }
          //

          //New Method
          // parse condition in a push by itself
          // if unsat, skip loop
          // if sat, traverse body (assignments already handled in assignment phrase)
          // parse condition again in a push by itself
          // check sat now
          // repeat if sat (parse body and condition again)
          // if unsat, pop and continue as usual
          solver.push()
          solver.push()
          var infCount = 0 // to avoid infinite loops
          val max_loops = 100
          var current = getBoolExpr(cond, funcVars)
          if(current != null) {
            solver.add(current)
            while (solver.check() == Status.SATISFIABLE && infCount < max_loops) { // this never stops
              expressions += current // adding assertion since it should be sat
              solver.pop()
              for (stat <- stats) {
                val (exprs, funcVars_temp) = traverseStat(stat, funcVars)
                if (!solvable) {
                  solver.pop()
                  return (ListBuffer(), null)
                }
                for (expr <- exprs) {
                  expressions += expr
                  solver.add(expr)
                }
                funcVars = funcVars_temp
              }
              solver.push()
              current = getBoolExpr(cond, funcVars)
              solver.add(current)
              infCount += 1
            }
          }
          solver.pop()
          solver.pop()
          if(infCount == max_loops){
            println("DISCLAIMER")
            println("There is a limit to the number of iterations a loop can take. ")
            println("Statement: " + stat)
            // we cant assume it is satisfiable, we must consider the whole condition as satisfiable
            println("If this limit is reached, it is assumed that the final iteration has negated the loop condition.")
          }
          expressions += ctx.mkNot(current) // adding final assignment negation since loop breaks so it obeys this

        // NEXT STEP
        // parse stats as normal (fix error above)
        // when assignment make sure to refer to previous variables (funcvars_n-1) in rhs, and lhs is new funcvars_n
        // dats the only thing i need to consider differently in the stats parsing, everything else remains unchagnged?
        // wait no after assignment, change the default funcvar used from _n-1 to _n so that the updated assignment is use dby default

        // should i only do it for simple loops only?

        // not condition and things after loop
        // but if before contradicts condition, it should not return unsat
        // if prev and condition unsat, simply skip whole block and condition
        // if they are fine, just copmare the not of the condition with anything after
        // the loop is guaranteed to end, and anything in the loop will not affect anything outside (in the case of simple loops)
        // simple loops = only variables present in the condition will be affected in the body

        // FOR LOOP
        // just do same stuff as function and repeat it
        // the no of repetitions is known already so the no of reps is known
        // do it like function

        //          var bodyExpr = getExpr(body, funcVars)
        //          ctx.mkImplies(loopCond, bodyExpr._1.asInstanceOf[Expr[BoolSort]])
        //            var block : ListBuffer[Expr[_]] = ListBuffer()
        //            for(bodyStat <- body) {
        //              block = block ++ traverseStat(bodyStat, )
        //            }
        //}
        // get the terms used in the condition
        // declare them with "_n" each time
        // check if sat first (do loop if sat)
        // do the loop on the new values each time
        // do block
        // assert condition with true
        // solve at this state? skip next loop when unsat

        case Term.Do(Term.Block(stats), cond) =>
          for(stat <- stats){
            val (exprs1, funcVars_temp1) = traverseStat(stat, funcVars)
            if(!solvable) return (ListBuffer(), null)
            for (expr1 <- exprs1) {
              expressions += expr1
              solver.add(expr1)
            }
            funcVars = funcVars_temp1
          }
          solver.push()
          solver.push()
          var infCount = 0 // to avoid infinite loops
          var current = getBoolExpr(cond, funcVars)
          val max_loops = 100
          if(current != null) {
            solver.add(current)
            while (solver.check() == Status.SATISFIABLE && infCount < max_loops) { // this never stops
              expressions += current // adding assertion since it should be sat
              solver.pop()
              for (stat <- stats) {
                val (exprs, funcVars_temp) = traverseStat(stat, funcVars)
                if (!solvable) {
                  solver.pop()
                  return (ListBuffer(), null)
                }
                for (expr <- exprs) {
                  expressions += expr
                  solver.add(expr)
                }
                funcVars = funcVars_temp
              }
              solver.push()
              current = getBoolExpr(cond, funcVars)
              solver.add(current)
              infCount += 1
            }
          }
          solver.pop()
          solver.pop()
          if(infCount == max_loops){
            println("DISCLAIMER")
            println("There is a limit to the number of iterations a loop can take. ")
            println("Statement: " + stat)
            // we cant assume it is satisfiable, we must consider the whole condition as satisfiable
            println("If this limit is reached, it is assumed that the final iteration has negated the loop condition.")
          }
          expressions += ctx.mkNot(current)
        // cant take array variables i guess
        case Term.Block(stats) =>
          for (stat <- stats) {
            var (exprs, funcVarsTemp) = traverseStat(stat, funcVars)
            if(!solvable) return (ListBuffer(), null)
            for (expr <- exprs) expressions += expr
          }
        case Term.Apply(Term.Name(name), args) => // only called by util // Term.Select(Term.Name("util"),
          if (utilFuncs.keySet.contains(name)) {
            isFromCond = false
            val funcExpr : BoolExpr = traverseBlock(name, args, funcVars)//, solver)
            expressions += funcExpr
          }
          else {
            println("Function does not exist")
            //pause()
          }


//          // this is function call within util function
//          // adds all interpretations as a list of assertions in the solver of the function
//          if (utilFuncs(name) != null) {
//            // new scope to store new assertions
//            solver.push() // utilSolver(name)
//            isFromCond = false
//            val interList = addFuncCall(name, args, funcVars)//, utilSolver(name))
//            for (inter <- interList) {
//              if (inter != null)
//                solver.add(inter.asInstanceOf[Expr[BoolSort]])
//            } // ------- this instance of may cause some issues but shouldnt really // utilSolver(name)
//            solver.pop() // utilSolver(name)
//          }
//          // RETURN EXPRESSIONS
        case _  =>
          println("DISCLAIMER")
          println("The statement the application is trying to parse is currently unsupported.")
          println("Statement: " + stat)
          // we cant assume it is satisfiable, we must consider the whole condition as satisfiable
          println("By default, we will assume that this branch is satisfiable to avoid accidental satisfiable branch removal")
          //pause()
          solvable = false

      }
      (expressions, funcVars)
    }

    //    def getTermNames(expr : Term) : List[String] = {
    //      // convert to string
    //      // find the first Term.Name
    //      // remove anything before, and the Term.Name( too
    //      // get whatever is in bracket
    //      // if it is not in the list, save it
    //      // if not skip
    //      // remove bracket and repeat process
    //      var stringExpr = expr.toString()
    //      while (stringExpr.nonEmpty) {
    //        if(stringExpr(0) == 'T'){
    //          val termDotName = stringExpr.substring(0, Math.min(stringExpr.length, 10))
    //          if (termDotName == "Term.Name("){
    //
    //          }
    //          else {
    //            stringExpr.drop(Math.min(stringExpr.length, 10))
    //          }
    //        }
    //        else {
    //          stringExpr.drop(0)
    //        }
    //      }
    //
    //    }

    // call this from function calls now
    def traverseBlock(funcName : String, args : List[Term], funcVars : Variables) : BoolExpr = {
      //var paramsExpr : ListBuffer[Expr[_]] = ListBuffer()
      // these will be irrelevent in the future when the function is being called
      // they will be rebuilt. these variables are only temporary until the model is retrieved
      val (params, stats) = utilFuncs(funcName)

      var utilParams : mutable.LinkedHashMap[String, (Expr[_], Int)] = mutable.LinkedHashMap()

      // ADD FUNCTION TO UTIL PARAMS
      //val newFunc : SortedMap[String, (Expr[_], Int)] = SortedMap()
      //utilParams = utilParams + newFunc // (funcName -> newFunc)

      for (param <- params) {
        param match {
          case Term.Param(mods, Term.Name(termName), Some(Type.Name(typ)), maybeTerm) =>
            if(typ == "String") {
              utilParams = addParams(utilParams, string, termName)
            }
            else if (typ == "Int") {
              utilParams = addParams(utilParams, integer, termName)
            }
            else if (typ == "Boolean")  {
              utilParams = addParams(utilParams, boolean, termName)
            }
            else println("THIS IS NOT A VALID PARAMETER TYPE")
          case _  =>
            println("Param of wrong syntax")
            //pause()
        }
      }

      // compare the parameters with the arguments here
      // check globals and stuff here
      var eqArgParam : ListBuffer[BoolExpr] = ListBuffer()
      if (args.length == utilParams.size) { // getLengthVars(utilParams(name)
        var counter = 0
        while (counter < args.length) {
          val arg = args(counter)
          val (argExpr, typTemp) = getExpr(arg, funcVars)
          if(!solvable) return null
          val param: Expr[_] = utilParams(utilParams.keys.toList(counter))._1 // getParam()
          //param.substitute????????????????????????????????????????????????????????????????????????????????
          eqArgParam += ctx.mkEq(argExpr, param)
          counter += 1
        }
      }
      else {
        println("NUMBER OF ARGUMENTS DON'T MATCH NUMBER OF PARAMETERS")
      }
      /////

      //utilParams = utilParams + (name -> paramsExpr.toList)

      var newFuncVars : Variables = new Variables() // parameters not included in funcVars
      for (param <- utilParams) {
        newFuncVars.vars = newFuncVars.vars + (param._1 -> param._2)
      }

      // i can do this, in the end it will all be one whole condition, but this is added separately before the condition so its not the part of the end
      // no wait this isnt a function
      //okok, to make function, get the function block in one whole boolean function
      //assert that bool expr equal to the name of the function
      // then insert the function into the main equation
      //solver.push()
      isFromCond = false
      var allExprsInBlock : ListBuffer[BoolExpr] = ListBuffer()
      for (stat <- stats) {
        val (exprs, _) = traverseStat(stat, newFuncVars)
        if(!solvable) return null
        // we dont need push and pop and stuff. the variables used will be from the funcVars (need to make sure this is correct)
        // add to list
        for(expr <- exprs) allExprsInBlock += expr
      }

      var agg = aggregate(allExprsInBlock)
      for(eq <- eqArgParam) agg = ctx.mkAnd(eq, agg)
      // aggregate and return
      agg
      //solver.pop()

      // GET THE STATS AND ASSERT
      // ALL FUNCTIONS ARE BOOLEAN SO ITS ALL OK
      // THIS FUNCTION IS NOT UNIT ANYMORE
    }

    // ------------------------------------------------------------------------------------------------------------

    def traverseDefns(defns : List[Stat]) : Unit = {
      // this could also include global variables in util file, should i include? Defn.Val
      for (defn <- defns) {
        defn match {
          // dont replace the names of the parameters to the arguments, just add an assertion to make sure they are equal
          case Defn.Def(a, Term.Name(name), b, List(params), typ, Term.Block(stats)) => // handle params by saving them as variables

            utilFuncs = utilFuncs + (name -> (params, stats))

          case _ =>
            println("TERM TYPE MISMATCH : Definition")
        }
      }
    }

    def traverseObject(obj : Defn.Object) : Unit = obj match { // doesnt return anything, just builds the map
      case Defn.Object(mods, name, Template(a, b, c, list)) =>
        traverseDefns(list)
      case _ =>
        println("TERM TYPE MISMATCH : Object(2)")
    }

    def traverseUtil(utilTree : Tree):Unit = {
      //println(utilTree)
      utilTree match{
        case Source(stats) =>
          global = false
          for (stat <- stats) {
            stat match {
              case obj @ Defn.Object(mods, name, template) =>
                traverseObject(obj)
              case Pkg(pkg, objs) =>
                objs.head match {
                  case obj @ Defn.Object(mods, name, template) =>
                    traverseObject(obj)
                }

            }
          }
        case _ =>
          println("TERM TYPE MISMATCH : Object")
      }

      //println("Util traversed ::")
      //for (solv <- utilSolver) {
      //      if(solver.check == Status.SATISFIABLE) { // solv._2
      //        val model = solver.getModel // solv._2
      //        println("Model " + solver + " : ") // solv._1
      //        println(model.toString)
      //        utilFuncs = utilFuncs + (solver -> model) // solv._1
      //        pause()
      //      }
      //      else {
      //        println("There is no model, function is unsatisfiable")
      //        utilFuncs = utilFuncs + (solver -> null) // solv._1
      //        pause()
      //        // no model assignment if unsat
      //      }
      //}

    } // for getting util specifically

    def traverse(conditionTree : Term):BoolExpr={
      global = true
      val conditionExpression = getBoolExpr(conditionTree, null)
      //println("Condition expression : " + conditionExpression)
      if(solvable && (conditionExpression != null)) {
        solver.add(conditionExpression)
        conditionExpression
      } else {
        println("~~ The current condition is not solvable. We hence assume satisfiability to avoid accidental deletion")
        val litBool = getBoolExpr(Lit.Boolean(true), null)
        solver.add(litBool) // assuming satisfiable condition
        litBool
      }
    } // call to traverse normal
  }

  def generateUtilFunctions(util : String) : Unit = {

    // needs to be parsed for each new condition combination
    var utilTree : Tree = null

    if (util.contains("def")) {
      //utilTree = toolbox.parse(util)
      utilTree = util.parse[Source].get
//      println(" ++++++++++++++ UTIL TREE ++++++++++++++")
//      //println(showRaw(utilTree))
//      println(utilTree.structure)
//      println(" +++++++++++++++++++++++++++++++++++++++")
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

      //pause()
      traverser.traverseUtil(utilTree)

    }
    else {
      utilTree = null
    }

//    println("Util traversed")
    //pause()
  }

  def generateFormulas(conditions : String, vars : Map[String, String]) : (Term, BoolExpr) = {
    // use toolbox to parse through command
    // use case matching to traverse the created tree recursively
    // in each case write the corresponding smtlib format command

    //pause()

    for (variable <- vars) {
      //      if (variable._2.contains("ean")) {
      //        smtlibVariables = smtlibVariables + "(declare-const " + variable._1 + " Bool)\n"
      //      }
      //      else {
      //        smtlibVariables = smtlibVariables + "(declare-const " + variable._1 + " " + variable._2 + ")\n"
      //      }
      variable._2 match {
        case "Int" =>
          addVars(integer, variable._1)
        case "Boolean" =>
          addVars(boolean, variable._1)
        case "String" =>
          addVars(string, variable._1)

      }
    }

    //pause()

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


    //val conditionTree = toolbox.parse(conditions)
    val conditionTree = conditions.parse[Term].get
    //    println(" ++++++++++++++ CONDITION TREE ++++++++++++++")
    //    //println(showRaw(conditionTree))
    //    println(conditionTree.structure)
    //    println(" ++++++++++++++++++++++++++++++++++++++++++++")
    //var utilTree = conditionTree // temporary value

    //pause()

    val conditionExp = traverser.traverse(conditionTree)

    //    println("Traversed")
    //pause()

    (conditionTree, conditionExp)
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

    // TO CHECK
    // check if this is returning the correct full model, because i think i am passing context in the parameters when i should be pointing to global vars of contexts
    // in that case pass name and if util or normal related context and solvers
    println("Checking the following ->")
    println(traverser.getSolver.toString)
    //pause()
    if(traverser.getSolver.check == Status.SATISFIABLE) {
      println("\nSAT ~~~~~~~~~~~~~~~~~~~\n")
      //pause()
      traverser.clearSolver()
      true
    }
    else {
      println("\nUNSAT ~~~~~~~~~~~~~~~~~~~\n")
      //pause()
      traverser.clearSolver()
      false
    }

  }

  def solveAgg(aggConds : BoolExpr) : Boolean = {
//    var condsBuffer : ListBuffer[BoolExpr] = ListBuffer()
//    for(cond <- conds) condsBuffer += cond
//    val aggConds = aggregate(condsBuffer)
    traverser.getSolver.add(aggConds)

    println("Checking the following (trace) ->")
    println(traverser.getSolver.toString)
    //pause()
    if(traverser.getSolver.check == Status.SATISFIABLE) {
      println("\nSAT ~~~~~~~~~~~~~~~~~~~\n")
      //pause()
      traverser.clearSolver()
      true
    }
    else {
      println("\nUNSAT ~~~~~~~~~~~~~~~~~~~\n")
      //pause()
      traverser.clearSolver()
      false
    }
  }

  // used for util
//  def checkSat() : Model  = {
//    if (traverser.getSolver.check == Status.SATISFIABLE){
//      traverser.getSolver.getModel
//    }
//    else {
//      null
//    }
//  }


}
