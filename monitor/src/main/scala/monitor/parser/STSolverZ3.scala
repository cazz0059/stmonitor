package monitor.parser

import smtlib.trees.Commands._
import scala.collection.mutable.ListBuffer
import scala.collection.SortedMap

import com.microsoft.z3._

import java.io._

//import scala.reflect.runtime._
//import scala.reflect.runtime.universe._
//import scala.tools.reflect.ToolBox

import scala.meta._

//import scala.sys.process._

//import z3

class STSolverZ3 {

//  private lazy val z3_path = {
//    sys.env.getOrElse("Z3_EXE", "z3.exe")
//  }

  //private val toolbox = currentMirror.mkToolBox()

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


  //private var smtlibVariables = ""
//  private var variablesInt : Map[String, IntExpr] = Map()
//  private var variablesBool : Map[String, BoolExpr] = Map()
//  private var variablesString : Map[String, Expr[SeqSort[BitVecSort]]] = Map()
  private var variables : Variables = new Variables()

  private var utilParams : Map[String, SortedMap[String, (Expr[_], Int)]] = Map() // Map[String, Variables]
  private var utilFuncs : Map[String, Model] = Map()
  //private var utilCtx : Map[String, Context] = Map()

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

  def addParams(name : String, typ : Int, variable : String) : Unit = {
    if(typ == integer) {
      println("Adding " + variable + " integer")
      val newParam : SortedMap[String, (Expr[_], Int)] = SortedMap((variable -> (ctx.mkIntConst(variable), typ)))
      val newParams : SortedMap[String, (Expr[_], Int)] = utilParams(name) ++ newParam
      utilParams = utilParams + (name -> newParams)// .integers = utilParams(name).integers + (variable -> ctx.mkIntConst(variable))
    }
    else if(typ == boolean) {
      val newParam : SortedMap[String, (Expr[_], Int)] = SortedMap((variable -> (ctx.mkBoolConst(variable), typ)))
      val newParams : SortedMap[String, (Expr[_], Int)] = utilParams(name) ++ newParam
      utilParams = utilParams + (name -> newParams)
    }
    else if(typ == string) {
      val newParam : SortedMap[String, (Expr[_], Int)] = SortedMap((variable -> (ctx.mkConst(ctx.mkSymbol(variable), ctx.mkStringSort()), typ)))
      val newParams : SortedMap[String, (Expr[_], Int)] = utilParams(name) ++ newParam
      //utilParams = utilParams + (utilParams(name) ++ (variable -> ctx.mkConst(ctx.mkSymbol(variable), ctx.mkStringSort())))
      utilParams = utilParams + (name -> newParams)
    }
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

  private val ctx: Context = new Context(new java.util.HashMap[String, String])

  // find way to eliminate conditions that have nothing to do with the unsatisfiability - saveUnsatConds()
  private var lemmas : List[String] = List() // SMT read this and see if it is the same semantics
  //private val cnfTransformer = new TransformToCnf()

  def pause() : Unit = {
    print("Pausing...")
    scala.io.StdIn.readLine()
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
    private var global = true
    private var isFromCond = false


    def createContext() : Unit = { // funcName : String
      // what to do with function overloading
      // no that s wayy too hard, add it to improvements
      //utilCtx = utilCtx + (funcName -> new Context(new java.util.HashMap[String, String]))
      solver.push()
      // thought i should make an array of variable decls for each function call with parameters and delcs within function
      // but keeping up with, for example, recursive functions, will just be a great complication
    }
    def removeContext() : Unit = { // funcName : String
      solver.pop()
      //utilParams = Map() // removing all parameters made. the parameters to be used will be created anew. Why? so that recursions do not clash in variables
      // no this is for moving on to getting the model of the next function, dont need other function model in this function, clear out params
    }
    def getSolver: com.microsoft.z3.Solver = {
      solver
    }
    def clearSolver() : Unit = {
      solver = ctx.mkSolver()
      count = 0
    }

    //private var utilSolver: Map[String, com.microsoft.z3.Solver] = Map()
//    def createUtilSolver(name : String) : Unit = {
//      utilSolver = utilSolver + (name -> utilCtx(name).mkSolver())
//    }
//    def getUtilSolver(name : String): com.microsoft.z3.Solver = {
//      utilSolver(name)
//    }
//    def clearUtilSolver() : Unit = {
//      utilSolver = Map()
//      count = 0
//    }

    def createFuncCall() : Variables = {
      // push
      // create variable set
      solver.push()
      new Variables()
    }

    def exitFuncCall() : Variables = {
      solver.pop()
      null // emptying variables we no longer need
    }

    def getVars(name : String, typ : Int) : Expr[_] = {
      variables.vars(name)._1
//      if (typ == integer) {
//        variables.integers(name)
//      }
//      else if (typ == boolean) {
//        variables.booleans(name)
//      }
//      else if (typ == string) {
//        variables.strings(name)
//      }
//      else null
    }

//    def getParam(name : String, funcName : String, typ : Int) : Expr[_] = {
//      // do smtn like getExpr for the parameters
//      if (typ == integer) {
//        utilParams(funcName).integers(name)
//      }
//      else if (typ == boolean) {
//        utilParams(funcName).booleans(name)
//      }
//      else if (typ == string) {
//        utilParams(funcName).strings(name)
//      }
//      else null
//    }

    def getFuncVars(funcVars : Variables, name : String, typ : Int) : Expr[_] = {
      funcVars.vars(name)._1
//      if (typ == integer) {
//        funcVars.integers(name)
//      }
//      else if (typ == boolean) {
//        funcVars.booleans(name)
//      }
//      else if (typ == string) {
//        funcVars.strings(name)
//      }
//      else null
    }

//    def getLengthVars(vars : Variables) : Int = {
//      vars.integers.size + vars.booleans.size + vars.strings.size
//    }

    private var count = 0
    def incCount() : Unit = {
      count+=1
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
    def getBoolExpr(arg : Term, funcVars : Variables) : BoolExpr = arg match {
      case Lit.Boolean(value) =>
        // cannot have this constant param name, there may be others in the same function
        val temp_bool_const = ctx.mkBoolConst("temp_traverse_bool_const_" + count)
        incCount()
        var temp_and : BoolExpr = null

        if(!value) temp_and = ctx.mkAnd(temp_bool_const, ctx.mkNot(temp_bool_const))
        else temp_and = temp_bool_const // why did i set it to null? because if it is itself it is always sat

        temp_and


      case termName @ Term.Name(name) =>
        traverseBoolVar(termName, funcVars)

      case infix @ Term.ApplyInfix(a, b, c, d) => traverseBoolInfix(infix, funcVars)

      case unary @ Term.ApplyUnary(o, a) => traverseBoolUnary(unary, funcVars)

      case funcCall @ Term.Apply(fun, args) =>
        traverseFuncBool(funcCall, funcVars)
        //null

      case _ =>
        println("TERM TYPE MISMATCH : BoolExpr")
        null
    }

    // call getArithExpr as well
    // update like the one above
    def getIntExpr(arg : Term, funcVars : Variables) : ArithExpr[_] = arg match {

      case Lit.Int(value) =>
        // return the value as an IntExpr
        println("Literal : Int")
        pause()
        val intVal : ArithExpr[_] = ctx.mkInt(value)
        intVal

      case term @ Term.Name(name) => traverseIntVar(term, funcVars)

      case term @ Term.ApplyInfix(a, b, c, d) =>
        //val arithExpr : IntExpr = getArithExpr(term, context, funcVars).asInstanceOf
        val arithExpr = getArithExpr(term, funcVars)
        println("Checking as instance of IntExpr from ArithExpr (infix)")
        pause()
        arithExpr

      case Term.ApplyUnary(o, a) =>
        val arithExpr = traverseIntUnary(Term.ApplyUnary(o, a), funcVars)
        println("Checking as instance of IntExpr from ArithExpr (unary)")
        pause()
        arithExpr

      case _ =>
        println("TERM TYPE MISMATCH : IntExpr")
        null
    }

    def getArithExpr(arg : Term, funcVars : Variables) : ArithExpr[_] = arg match {
      case Term.ApplyInfix(a, b, c, d) => traverseArithInfix(Term.ApplyInfix(a, b, c, d), funcVars)
      case _ =>
        println("TERM TYPE MISMATCH : ArithExpr")
        null
    }

    def getStringExpr(arg : Term, funcVars : Variables) : Expr[SeqSort[BitVecSort]] = arg match {
      case term @ Term.Name(name) => traverseStringVar(term, funcVars)
      case _ =>
        println("TERM TYPE MISMATCH : StringExpr")
        null
    }

    def getExpr(arg : Term, funcVars : Variables) : (Expr[_], Int) = {
      println("Getting expression")

      var expr : Expr[_] = getBoolExpr(arg, funcVars)
      var typ = boolean
      if (expr == null) {
        expr = getIntExpr(arg, funcVars)
        typ = integer
      }
//      if (expr == null) {
//        println("Getting arith infix expression...")
//        expr = getArithExpr(arg, context, funcVars)
//        typ = integer
//      }
      if (expr == null) {
        expr = getStringExpr(arg, funcVars)
        typ = string
      }
      if (expr == null) {
        println("ERROR : EXPR NOT FOUND")
        return null
      }
      println("Expression retrieved")
      (expr, typ)
    }

    def interpret(name : String, interpretation : Expr[_]) : BoolExpr = {
      // FIX TO ACCEPT ARITHMETIC ASSERTIONS AS WELL
      if(interpretation == null) {
        println("INTERPRETATION IS NULL")
        return null
      }
      val inter = interpretation.toString
      val nameConst = ctx.mkBoolConst(name)
      if (inter == "true") {
        ctx.mkAnd(nameConst, nameConst)
      }
      else if (inter == "false") {
        ctx.mkAnd(nameConst, ctx.mkNot(nameConst))
      }
      else {
        println("INTERPRETATION MATCHES NOTHING")
        null
      }
    }

//    def interpret(name : String, interpretation : Int, context : Context) : IntExpr = {
//
//    }

    // called from traverse func call, which is called by get bool expr
    def addFuncCall(name : String, args : List[Term], funcVars : Variables) : ListBuffer[Expr[_]] = {
      val func = utilFuncs(name) // getting the model of the function
      // creating new scope for function
      // issue when scoping if there are vars in conds and in util func with same name
      // another limitation : util functions are all of type bool
      //solv.push()
      //val boolFunc = getBoolExpr(func, context)
//      func match {
//        case boolFunc @ BoolExpr =>
//          solv.add(boolFunc)
//        case _ =>
//          println("Util function not boolean type")
//          pause()
//      }
       // ----------------------------------------------------------------------------------------------------------
      // handling parameters
      //var paramDecls : Array[Expr[_]] = Array()
      val constDecls = func.getConstDecls // getting all declarations made within the function (not inc params - params inc coz of decl in util building)
      println("All const decl >>")
      for (c <- constDecls)
        println(c.toString)

      println("Params of " + name)
      println(utilParams(name))

      var interList : ListBuffer[Expr[_]] = ListBuffer() // to store list of interpretations (model of each variable)

      // check that the lists are the same size, and use a counter to iterate and match param with arg
      if (args.length == utilParams(name).size) { // getLengthVars(utilParams(name)
        var counter = 0
        while (counter < args.length) {
          val arg = args(counter)
          println("getting arg and param")
          val (argExpr, typTemp) = getExpr(arg, funcVars)
          println("got arg " + argExpr.toString)
          val param : Expr[_] = utilParams(name)(utilParams(name).keys.toList(counter))._1 // getParam()
          println("got param " + param.toString)

          // params needs to be created when util parsed
          // they need to be part of the model
          // then be able to get the interpretations only if they are part of the model
          // so need to be assigned before
          // need method to retrieve the expressions to make the argument equality assertion

//          val paramExpr : Expr[_] = param match {
//            case Term.Param(mods, Term.Name(name), Some(Type.Name(typ)), maybeTerm) =>
//              if (typ == "String") context.mkConst(context.mkSymbol(name), context.mkStringSort()) //context.mkString(name) // do i need to save this?
//              else if (typ == "Int") context.mkIntConst(name)
//              else if (typ == "Boolean")  context.mkBoolConst(name)
//              else {
//                println("THIS IS NOT A VALID PARAMETER TYPE")
//                null
//              }
//            case _  =>
//              println("Param of wrong syntax")
//              pause()
//              null
//          }
          // adding assert to context that arg is eq to param
          //val paramExpr : Expr[_] = null //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! get the expression from the constDecl
          //paramDecls = paramDecls :+ paramExpr

          // context mismatch big issue
          // there s no way to cmpare the elemts in the function context model with the normal one
          // but we cant remove the function context as we need the model
          // possible solution : we will have to parse util each time and treat it as if it were in the same context
          // so, removing all the util_ maps, no interpretations or whatsoever, and run the util within the condition rather than before
          // of course this is a time and resrouce waster
          // but if i only run a single condition once, like i plan on doing eventually, this can happen better i guess?
          // still need to run it each time its called
          // cant do that, context changes for each condition
          // ok find out how to make subcontexts, then can make utilContexts out of those, and add them to main context
          // cant do multithreading as z3 doesnt support it fully
          // use simplify to simplify them?
          // remember that z3 can backtrack, so can check utils in the beginning and then simplify/remove them and use them later maybe?

          // idea - do all utils in ctx, then get interpretation and stuff and save them (all in ctx), then just clear the whole thing, interpretation and model still saved

          val equality : BoolExpr = ctx.mkEq(argExpr, param)
          println("Parameter setting >> " + equality)
          pause()

//          var inter = null
//          for (constDecl <- constDecls){
//            if (constDecl.getName == param.name) {
//              inter = func.getConstInterp(constDecl)
//              inter = inter.toString.replace(param.name, arg.name)
//            }
//          }
          // turn it to string
          // change name there
          // change it back?


          // interpret separately
          // for each arg
          //   get interpretation of corresponding parameter
          // save it as the corresponding arg's interpretation
          // NEVER MIND BECAUSE ARGS CAN BE ANYTHING
          // JUST ADD ASSERTION THAT ARG == PARAM

          // getting interpretation of param (without previous assertion)
          val paramFunc : FuncDecl[_] = param.getFuncDecl //!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! get funcdecl from the constDecl
          println("Parameter Decl: " + paramFunc.toString)
          //pause()
          val interpretation = func.getConstInterp(paramFunc) // this is an IDE issue
          println("Parameter Interpretation: " + interpretation)

          val inter = interpret(name, interpretation)
          interList+=inter
          //        if (inter != null)
          //          solv.add(inter)
          println("Added " + name + " to interpretations")
          //pause()

          counter+=1
        }
      }
      else {
        println("NUMBER OF ARGUMENTS DON'T MATCH NUMBER OF PARAMETERS")
      }


      // -------------------------------------


      for (constDecl <- constDecls) { // rebuild declarations by adding constants
        println("Constant Decl: " + constDecl.toString)
        //pause()
        val name = constDecl.getName.toString
        ctx.mkBoolConst(name) // find out how to do ints as well
        //val declKind = constDecl.getDeclKind
        //println("Decl Kind: " + declKind)
        //if ()
        val interpretation = func.getConstInterp(constDecl) // this is an IDE issue
        println("Interpretation: " + interpretation)

        val inter = interpret(name, interpretation)
        println("after inter method :: " + inter)
        interList+=inter
//        if (inter != null)
//          solv.add(inter)
        println("Added " + name + " to interpretations")
        //pause()
      }
      // get all the declarations
      // replace the parameters - change the names in the function, beware overriding variables in the function,
      //         fix this by changing the name of the declaration within the function to not match that outside
      // get interpretation for each declaration

      //solv.pop()
      println()
      interList = interList.filter(_ != null)
      println("InterList :::")
      println(interList)
      interList
    }

    // remember that embedded operations exist, so i cant just put "assert" in front of all the operators
    //    try and build the conditions (select and apply) recursively before adding the assert
    // use smt-lib List (recursive) some combination of that to unfold a recursive function???
    // or Tree??
    // z3 cannot prove by induction, so unfolding needs to take place to prove by deduction
    def traverseBoolVar(term : Term.Name, funcVars : Variables) : BoolExpr = term match {
      // probably is a singular boolean variable, as in any other case it would form part of a formula
      case Term.Name(name) =>
        println("Term bool name " + name + " ##")
        if(global || isFromCond) {
          if (variables.vars.keySet.contains(name) && variables.vars(name)._2 == boolean) { // .integers // isInstanceOf[BoolExpr]
            println("Keyset : " + variables.vars.keySet)
            variables.vars(name).asInstanceOf[BoolExpr]
          } else { // search also in parameters and in decls within function
            // see how to differentiate between all the variables' names if they are the same and all
            null
          }
        }
        else {
          print("Func vars : " + funcVars)
          if (funcVars.vars.keySet.contains(name) && funcVars.vars(name)._2 == boolean) { // .integers
            println("Keyset : " + funcVars.vars.keySet)
            funcVars.vars(name).asInstanceOf[BoolExpr]
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
        println("Term int name " + name + " ##")

        if(global || isFromCond) {
          if (variables.vars.keySet.contains(name) && variables.vars(name)._2 == integer) { // .integers
            println("Keyset : " + variables.vars.keySet)
            variables.vars(name).asInstanceOf[IntExpr]
          } else { // search also in parameters and in decls within function
            // see how to differentiate between all the variables' names if they are the same and all
            null
          }
        }
        else { // only come here is called from util function
          if (funcVars.vars.keySet.contains(name) && funcVars.vars(name)._2 == integer) { // .integers
            println("Keyset : " + funcVars.vars.keySet)
            funcVars.vars(name).asInstanceOf[IntExpr]
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
          println("Term string name " + name + " ##")
          if (variables.vars.keySet.contains(name) && variables.vars(name)._2 == string) { // .integers
            println("Keyset : " + variables.vars.keySet)
            variables.vars(name).asInstanceOf[Expr[SeqSort[BitVecSort]]]
          } else { // search also in parameters and in decls within function
            // see how to differentiate between all the variables' names if they are the same and all
            null
          }
        }
        else {
          if (funcVars.vars.keySet.contains(name) && funcVars.vars(name)._2 == string) { // .integers
            println("Keyset : " + funcVars.vars.keySet)
            funcVars.vars(name).asInstanceOf[Expr[SeqSort[BitVecSort]]]
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
        println("Term apply infix " + lhs.toString() + " && " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsBool = getBoolExpr(lhs, funcVars)
        for (arg <- args) {
          val rhsBool = getBoolExpr(arg, funcVars)
          if (rhsBool != null)
            lhsBool = ctx.mkAnd(lhsBool, rhsBool)
        }
        lhsBool

      case Term.ApplyInfix(lhs, Term.Name("||"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " || " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsBool = getBoolExpr(lhs, funcVars)
        for (arg <- args) {
          val rhsBool = getBoolExpr(arg, funcVars)
          if (rhsBool != null)
            lhsBool = ctx.mkOr(lhsBool, rhsBool)
        }
        lhsBool

      case Term.ApplyInfix(lhs, Term.Name("<"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " < " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        val lhsInt = getIntExpr(lhs, funcVars)
        val rhsInt = getIntExpr(args.head, funcVars)
        println("Checking lhs: " + lhsInt + " and rhs: " + rhsInt)
        val lt = ctx.mkLt(lhsInt.asInstanceOf[Expr[ArithSort]], rhsInt.asInstanceOf[Expr[ArithSort]])
        println("Testing as instance of: " + lt)
        //pause()
        lt

      case Term.ApplyInfix(lhs, Term.Name(">"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " > " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        val lhsInt = getIntExpr(lhs, funcVars)
        val rhsInt = getIntExpr(args.head, funcVars)
        println("Applying operator > ...")
        val gt = ctx.mkGt(lhsInt.asInstanceOf[Expr[ArithSort]], rhsInt.asInstanceOf[Expr[ArithSort]])
        println("Testing as instance of: " + gt)
        //pause()
        gt

      case Term.ApplyInfix(lhs, Term.Name("<="), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " <= " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        val lhsInt = getIntExpr(lhs, funcVars)
        val rhsInt = getIntExpr(args.head, funcVars)
        val lte = ctx.mkLe(lhsInt.asInstanceOf[Expr[ArithSort]], rhsInt.asInstanceOf[Expr[ArithSort]])
        println("Testing as instance of: " + lte)
        //pause()
        lte

      case Term.ApplyInfix(lhs, Term.Name(">="), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " >= " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        val lhsInt = getIntExpr(lhs, funcVars)
        val rhsInt = getIntExpr(args.head, funcVars)
        val gte = ctx.mkGe(lhsInt.asInstanceOf[Expr[ArithSort]], rhsInt.asInstanceOf[Expr[ArithSort]])
        println("Testing as instance of: " + gte)
        //pause()
        gte

      case Term.ApplyInfix(arg, Term.Name("=="), tArgs, args) =>
        println("Term apply infix " + arg.toString() + " == " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhs : Expr[_] = getIntExpr(arg, funcVars)
        var rhs : Expr[_] = null
        if(lhs != null) {
          rhs = getIntExpr(args.head, funcVars)
        }
        else {
          lhs = getBoolExpr(args.head, funcVars)
          rhs = getBoolExpr(args.head, funcVars)
        }
        val gte = ctx.mkEq(lhs, rhs)
        println("Testing as instance of: " + gte)
        //pause()
        gte

      case _ =>
        null
    }

    def traverseArithInfix(term : Term.ApplyInfix, funcVars : Variables) : ArithExpr[_] = term match {
      case Term.ApplyInfix(lhs, Term.Name("+"), tArgs, args) =>
        // has to be recursive over the term names
        println("Term apply infix " + lhs.toString() + " + " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsInt : ArithExpr[_] = ctx.mkAdd(getIntExpr(lhs, funcVars)) // ctx.mkIntConst("0")
        for (arg <- args){
          println("Arg : "  + arg)
          // checking if there is a function call (that returns a boolean)
          val rhsInt = getIntExpr(arg, funcVars)
          if (rhsInt != null)
            lhsInt = ctx.mkAdd(lhsInt, rhsInt)
        }
        println("Infix applied")
        lhsInt

      case Term.ApplyInfix(lhs, Term.Name("-"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " - " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsInt : ArithExpr[_] = ctx.mkAdd(getIntExpr(lhs, funcVars))
        for (arg <- args) {
          val rhsInt = getIntExpr(arg, funcVars)
          if (rhsInt != null)
            lhsInt = ctx.mkSub(lhsInt, rhsInt)
        }
        lhsInt

      case Term.ApplyInfix(lhs, Term.Name("*"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " * " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsInt : ArithExpr[_] = ctx.mkAdd(getIntExpr(lhs, funcVars))
        for (arg <- args) {
          val rhsInt = getIntExpr(arg, funcVars)
          if (rhsInt != null)
            lhsInt = ctx.mkMul(lhsInt, rhsInt)
        }
        lhsInt

      case Term.ApplyInfix(lhs, Term.Name("/"), tArgs, args) =>
        println("Term apply infix " + lhs.toString() + " / " + args.head.toString())
        if(args.length > 1)
          println("Note: There are more args in this infix")

        var lhsInt : ArithExpr[_] = ctx.mkAdd(getIntExpr(lhs, funcVars))
        for (arg <- args) {
          val rhsInt = getIntExpr(arg, funcVars)
          if (rhsInt != null)
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
        ctx.mkNot(getBoolExpr(arg, funcVars))
      case _ =>
        null
    }

    def traverseIntUnary(term : Term.ApplyUnary, funcVars : Variables) : ArithExpr[_] = term match {
      case Term.ApplyUnary(null, arg) =>
        getIntExpr(arg, funcVars)
      case Term.ApplyUnary(op, arg) =>
        println("Term unary " + arg)
        ctx.mkSub(null, getIntExpr(arg, funcVars)) // ctx.mkBVNeg(ctx.mkInt2BV(getIntExpr(arg)))
      case _ =>
        null
    }

    ///--------------------------------
    def aggregateInter(interList : ListBuffer[Expr[_]]) : BoolExpr = {
      if(interList.tail.nonEmpty)
        ctx.mkAnd(interList.head.asInstanceOf[Expr[BoolSort]], aggregateInter(interList.tail))
      else
        interList.head.asInstanceOf[BoolExpr]
    }

    // called only from conditions, not util file
    // gets list of all interpretations in the function call as a big AND of assertions in BoolExpr format, called from getBoolExpr to be used anywhere
    def traverseFuncBool(app : Term.Apply, funcVars : Variables) : BoolExpr = app match { // does the adding itself
      case Term.Apply(Term.Select(Term.Name("util"), Term.Name(name)), args) =>
        // this is function call within util function
        println("Getting bool function call")
        var interAgg : BoolExpr = null
        solver.push() // making a new scope like below function, it has to be "solve" since its a scope from the universal conditions
        //if (utilFuncs(name) != null) {
        if (utilFuncs.keySet.contains(name)) {
          global = false
          isFromCond = true
          val interList = addFuncCall(name, args, funcVars)//, solver)
          global = true
          println("List of interpretations :::")
          println(interList)
          interAgg = aggregateInter(interList)
          println("InterAgg :::")
          println(interAgg)
        }
        solver.pop()
        println("InterAgg :::")
        println(interAgg)
        pause()
        interAgg
      case _  =>
        println("Function invalid")
        pause()
        null
    }
    ///-----------------------------------

    def traverseStat(stat : Stat, fVars : Variables) : Unit = {
      var funcVars = fVars
      stat match{
        case bool @ Lit.Boolean(value) =>
          // add assert statement
          solver.add(getBoolExpr(bool, null)) // utilSolver(name) // funcVars is null wont make a difference because its just a boolean literal
        case Defn.Val(mods, List(Pat.Var(Term.Name(name))), decltpe, rhs) =>
          println("Getting val")
          val (getRHS, typTemp)  = getExpr(rhs, funcVars)
          //val getRHS : Sort = getRHSTemp.getSort.asInstanceOf[Sort] // : Sort
          println("Val Expression : " + getRHS)
          // fix this
          // wait adding things to context doesnt change stuff
          // like adding multiple adds and not to the same expression
          // now just add the assert equals
          // z3 cannot assign no

          // the assertions of the rhs are already being asserted with the context, maybe this should be changed so its just an assignment rather than an assertion
          //context.mkConst(name, getRHS)
          funcVars = addFuncVars(funcVars, typTemp, name) // adding the variable to the list (not the rhs, just the name and a const with it)
          ctx.mkEq(getFuncVars(funcVars, name, typTemp), getRHS) // "assigning" the rhs to the created (above) constant
          pause()
        case Defn.Var(mods, List(Pat.Var(Term.Name(name))), decltpe, rhs) =>
          val (getRHS, typTemp)  = getExpr(rhs.get, funcVars)
          //val getRHS : Sort = getRHSTemp.getSort.asInstanceOf[Sort] // : Sort
          println("Var Expression : " + getRHS)
          //context.mkConst(name, getRHS)
          funcVars = addFuncVars(funcVars, typTemp, name)
          ctx.mkEq(getFuncVars(funcVars, name, typTemp), getRHS)
          pause()
        case Term.Assign(Term.Name(lhsName), rhs) =>
          // only func vars can be changed
          // just change the name of the fuc vars
          // give them numbers for every assignment
          //var lhsExpr = getExpr(lhs, funcVars)
          val lhsExpr = funcVars.vars(lhsName)
          addFuncVars(funcVars, lhsExpr._2, lhsName + "_" + count); incCount()
          val rhsExpr = getExpr(rhs, funcVars)._1
          // assert equals
          ctx.mkEq(lhsExpr._1, rhsExpr)
          // change name

        case infix @ Term.ApplyInfix(lhs, op, targs, args) =>
          solver.add(getBoolExpr(infix, funcVars))
          pause()
        case Term.While(expr, body) =>
          println("Body ###")
          println(body.toString())
          // must unroll, no blocks and pushes and pops
          // must change all the decls (as per example)
          var count = 0
        //{
          var funcVarsTemp: Variables = new Variables()
          for (funcVar <- funcVars.vars) { // new set of variables
            val funcVarNameTemp = funcVar._1 + "_" + count
            val funcVarValTemp = funcVar._2
            funcVarsTemp.vars = funcVarsTemp.vars + (funcVarNameTemp -> funcVarValTemp)
            count = count + 1
          }
          var loopCond = getBoolExpr(expr, funcVars)
          body match {
            case Term.Block(stats) =>
              var block : ListBuffer[Expr[_]] = ListBuffer()
              for(bodyStat <- stats) {
                //block = block + traverseStat(bodyStat, funcVarsTemp)
              }
          }
          // NEXT STEP
          // parse stats as normal (fix error above)
          // when assignment make sure to refer to previous variables (funcvars_n-1) in rhs, and lhs is new funcvars_n
          // dats the only thing i need to consider differently in the stats parsing, everything else remains unchagnged?
          // wait no after assignemnt, change the default funcvar used from _n-1 to _n so that the updated assignment is use dby default

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

          var bodyExpr = getExpr(body, funcVars)
          ctx.mkImplies(loopCond, bodyExpr._1.asInstanceOf[Expr[BoolSort]])
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
        case Term.For(enums, body) =>
          println(enums)
          for(enum <- enums) {
            println(enum)
          }
        case Term.Apply(Term.Select(Term.Name("util"), Term.Name(name)), args) => // only called by util
          // this is function call within util function
          // adds all interpretations as a list of assertions in the solver of the function
          if (utilFuncs(name) != null) {
            // new scope to store new assertions
            solver.push() // utilSolver(name)
            isFromCond = false
            val interList = addFuncCall(name, args, funcVars)//, utilSolver(name))
            println("List of interpretations :::")
            println(interList)
            for (inter <- interList) {
              if (inter != null)
                solver.add(inter.asInstanceOf[Expr[BoolSort]])
            } // ------- this instance of may cause some issues but shouldnt really // utilSolver(name)
            solver.pop() // utilSolver(name)
          }
        case _  =>
          println("Statement does not exist")
          pause()

      }
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
//      print(stringExpr)
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

    // function contents
    def traverseBlock(funcName : String, params : List[Term.Param], stats : List[Stat]) : Unit = {
      //var paramsExpr : ListBuffer[Expr[_]] = ListBuffer()
      // these will be irrelevent in the future when the function is being called
      // they will be rebuilt. these variables are only temporary until the model is retrieved
      println("Traversing block ...")
      // ADD FUNCTION TO UTIL PARAMS
      val newFunc : SortedMap[String, (Expr[_], Int)] = SortedMap()
      utilParams = utilParams + (funcName -> newFunc)
      for (param <- params) {
        println("Current param ... " + param.toString())
        param match {
          case Term.Param(mods, Term.Name(termName), Some(Type.Name(typ)), maybeTerm) =>
            if(typ == "String") {
              //paramsExpr += context.mkConst(context.mkSymbol(termName), context.mkStringSort())
              addParams(funcName, string, termName)
            } // do i need to save this?
            else if (typ == "Int") {
              //paramsExpr += context.mkIntConst(termName)
              println("Traversing integer parameter ...")
              println("funcName : " + funcName)
              println("termName : " + termName)
              addParams(funcName, integer, termName)
            }
            else if (typ == "Boolean")  {
              //paramsExpr += context.mkBoolConst(termName)
              addParams(funcName, boolean, termName)
            }
            else println("THIS IS NOT A VALID PARAMETER TYPE")
          case _  =>
            println("Param of wrong syntax")
            pause()
        }
      }

      println("Parameters saved ...")
      //utilParams = utilParams + (name -> paramsExpr.toList)

      var funcVars : Variables = new Variables() // parameters not included in funcVars
      for (param <- utilParams(funcName)) {
        funcVars.vars = funcVars.vars + (param._1 -> param._2)
      }

      for (stat <- stats) {
        println("Current statement ... " + stat.toString())
        traverseStat(stat, funcVars)
      }
    }

    def traverseDefns(defns : List[Stat]) : Unit = {
      // this could also include global variables in util file, should i include? Defn.Val
      for (defn <- defns) {
        println("Traversing defn ...")
        defn match {
            // dont replace the names of the parameters to the arguments, just add an assertion to make sure they are equal
          case Defn.Def(a, Term.Name(name), b, List(params), typ, Term.Block(stats)) => // handle params by saving them as variables
            createContext() // add context for function
            //createUtilSolver(name)
            traverseBlock(name, params, stats) // traverse function and add assertions

            // checking sat and getting model
            if(solver.check == Status.SATISFIABLE) { // solv._2
              val model = solver.getModel // solv._2
              println("Model " + name + " : ") // solv._1
              println(model.toString)
              // saving only the model
              utilFuncs = utilFuncs + (name -> model) // solv._1
              pause()
            }
            else {
              println("There is no model, function is unsatisfiable")
              utilFuncs = utilFuncs + (name -> null) // solv._1
              pause()
              // no model assignment if unsat
            }


            removeContext()

          case _ =>
            println("TERM TYPE MISMATCH : Definition")
        }
      }
    }

    def traverseObject(obj : Defn.Object) : Unit = obj match { // doesnt return anything, just builds the map
      case Defn.Object(mods, name, Template(a, b, c, list)) =>
        println("Traversing object ...")
        traverseDefns(list)
      case _ =>
        println("TERM TYPE MISMATCH : Object(2)")
    }

    def traverseUtil(utilTree : Tree):Unit = {
      utilTree match{
        case Source(stats) =>
          global = false
          println("Traversing util ...")
          for (stat <- stats) {
            stat match {
              case obj @ Defn.Object(mods, name, template) =>
                traverseObject(obj)
            }
          }
        case _ =>
          println("TERM TYPE MISMATCH : Object")
      }

      println("Util traversed ::")
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

    def traverse(conditionTree : Term):Unit={
      global = true
      solver.add(getBoolExpr(conditionTree, null))
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

//      for (context <- utilCtx.toIterator) {
//        println("Function : " + context._1)
//        println(traverser.getUtilSolver(context._1).toString)
//        println()
//      }

    }
    else {
      utilTree = null
    }

    println("Util traversed")
    pause()
  }

  def generateFormulas(conditions : String, vars : Map[String, String]) : Unit = {
    // use toolbox to parse through command
    // use case matching to traverse the created tree recursively
    // in each case write the corresponding smtlib format command

    println("Variables")
    println(vars)
    pause()

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

    println("All variables >>>")
    println(variables.vars)
//    println(variables.booleans)
//    println(variables.strings)
    pause()

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

    // TO CHECK
    // check if this is returning the correct full model, because i think i am passing context in the parameters when i should be pointing to global vars of contexts
    // in that case pass name and if util or normal related context and solvers
    println("Checking the following ->")
    println(traverser.getSolver.toString)
    pause()
    if(traverser.getSolver.check == Status.SATISFIABLE) {
      println("\nSAT ~~~~~~~~~~~~~~~~~~~\n")
      pause()
      traverser.clearSolver()
      true
    }
    else {
      println("\nUNSAT ~~~~~~~~~~~~~~~~~~~\n")
      pause()
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
