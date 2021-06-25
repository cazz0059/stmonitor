package monitor.parser

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import com.microsoft.z3._

import scala.meta.contrib.XtensionTreeEquality
import scala.meta._
import scala.language.existentials

class STSolverZ3 {

  var solvable = true // turns to false when a function is unsolvable

  // enumeration
  val integer = 0
  val boolean = 1
  val string = 2

  class Variables {
    var vars : Map[String, (Expr[_], Int)] = Map() // name -> (expression, type (enum))
  }

  private val ctx: Context = new Context(new java.util.HashMap[String, String])
  private var variables : Variables = new Variables()

  private var utilFuncs : Map[String, (List[Term.Param], List[Stat])] = Map() // params, stats in block

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
      val newParam : (String, (Expr[_], Int)) = variable -> (ctx.mkIntConst(variable), typ)
      utilParams += newParam
    }
    else if(typ == boolean) {
      val newParam : (String, (Expr[_], Int)) = variable -> (ctx.mkBoolConst(variable), typ)
      utilParams += newParam
    }
    else if(typ == string) {
      val newParam : (String, (Expr[_], Int)) = variable -> (ctx.mkConst(ctx.mkSymbol(variable), ctx.mkStringSort()), typ)
      utilParams += newParam
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
  private var lemmas : List[Set[Term]] = List()

  def addAssertionLabel(label : String, assertion : BoolExpr) : Unit = {
    assertions = assertions + (label -> assertion)
  }

  def getLemmas : List[Set[Term]] = {
    lemmas
  }

  def addLemma(unsatCond : Set[Term]) : Unit = {
    lemmas = unsatCond :: lemmas
  }

  def compareToLemmas(checkCond : Set[Term]) : Boolean = {
    for(lemma <- lemmas) {

      var equCheck = true // checks that all elts in set are equal
      for(lm <- lemma){
        var currLemmaCheck = false
        for(cond <- checkCond){
          if(lm.isEqual(cond)) {
            currLemmaCheck = true
          }
        }
        equCheck = equCheck && currLemmaCheck
      }

      if(equCheck) {
        return false
      }
    }
    true
  }

  object traverser extends Traverser {
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


    def getFuncVars(funcVars : Variables, name : String) : (Expr[_], Int) = {

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

    def getBoolExpr(arg : Term, funcVars : Variables) : BoolExpr = arg match {
      case Lit.Boolean(value) =>
        if(value)
          ctx.mkTrue()
        else
          ctx.mkFalse()

      case termName @ Term.Name(name) =>
        traverseBoolVar(termName, funcVars)

      case infix @ Term.ApplyInfix(a, b, c, d) => traverseBoolInfix(infix, funcVars)

      case unary @ Term.ApplyUnary(o, a) => traverseBoolUnary(unary, funcVars)

      case funcCall @ Term.Apply(fun, args) =>
        var funcBool = traverseFuncBool(funcCall, funcVars)
        if(funcBool == null) solvable = false
        funcBool

      case _ =>
        solvable = false
        null
    }

    def getIntExpr(arg : Term, funcVars : Variables) : ArithExpr[_] = arg match {

      case Lit.Int(value) =>
        val intVal : ArithExpr[_] = ctx.mkInt(value)
        intVal

      case term @ Term.Name(name) => traverseIntVar(term, funcVars)

      case term @ Term.ApplyInfix(a, b, c, d) =>
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
      if (expr == null) {
        expr = getStringExpr(arg, funcVars)
        typ = string
      }
      if (expr == null) {
        println("ERROR : EXPR NOT FOUND, Solvable? " + solvable)
        return (null, -1)
      }
      solvable = true
      (expr, typ)
    }

    def traverseBoolVar(term : Term.Name, funcVars : Variables) : BoolExpr = term match {
      case Term.Name(name) =>
        if(global || isFromCond) {
          if (variables.vars.keySet.contains(name) && variables.vars(name)._2 == boolean) { // .integers // isInstanceOf[BoolExpr]
            variables.vars(name)._1.asInstanceOf[BoolExpr]
          } else {
            null
          }
        }
        else {
          val boolVar = getFuncVars(funcVars, name)
          if (boolVar != null && boolVar._2 == boolean) {
            boolVar._1.asInstanceOf[BoolExpr]
          } else {
            null
          }
        }
      case _ =>
        null
    }

    def traverseIntVar(term : Term.Name, funcVars : Variables) : IntExpr = term match {
      case Term.Name(name) =>

        if(global || isFromCond) {
          if (variables.vars.keySet.contains(name) && variables.vars(name)._2 == integer) { // .integers
            variables.vars(name)._1.asInstanceOf[IntExpr]
          } else {
            null
          }
        }
        else {
          val intVar = getFuncVars(funcVars, name)
          if (intVar != null && intVar._2 == integer) {
            intVar._1.asInstanceOf[IntExpr]
          } else {
            null
          }
        }

      case _ =>
        null
    }

    def traverseStringVar(term : Term.Name, funcVars : Variables) : Expr[SeqSort[BitVecSort]] = term match {
      case Term.Name(name) =>
        if(global || isFromCond) {
          if (variables.vars.keySet.contains(name) && variables.vars(name)._2 == string) {
            variables.vars(name)._1.asInstanceOf[Expr[SeqSort[BitVecSort]]]
          } else {
            null
          }
        }
        else {
          val stringVar = getFuncVars(funcVars, name)
          if (stringVar != null && stringVar._2 == string) {
            stringVar._1.asInstanceOf[Expr[SeqSort[BitVecSort]]]
          } else {
            null
          }
        }

      case _ =>
        null
    }

    def traverseBoolInfix(term : Term.ApplyInfix, funcVars : Variables) : BoolExpr = term match {
      case Term.ApplyInfix(lhs, Term.Name("&&"), tArgs, args) =>
        var lhsBool = getBoolExpr(lhs, funcVars)
        if(lhsBool != null) {
          for (arg <- args) {
            val rhsBool = getBoolExpr(arg, funcVars)
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
          ctx.mkUnaryMinus(unExp)
        }
        else
          null
      case _ =>
        null
    }

    def traverseFuncBool(app : Term.Apply, funcVars : Variables) : BoolExpr = app match {
      case Term.Apply(Term.Select(Term.Name("util"), Term.Name(name)), args) =>
        if (utilFuncs.keySet.contains(name)) {
          global = false
          isFromCond = true
          val funcExpr : BoolExpr = traverseBlock(name, args, funcVars)//, solver)
          global = true
          funcExpr
        }
        else {
          println("Function does not exist")
          null
        }
      case _  =>
        println("Function invalid")
        null
    }

    def traverseStat(stat : Stat, fVars : Variables) : (ListBuffer[BoolExpr], Variables) = {
      var funcVars = fVars
      var expressions : ListBuffer[BoolExpr] = ListBuffer()

      stat match{
        case bool @ Lit.Boolean(value) =>
          var boolExpr = getBoolExpr(bool, null)
          if(boolExpr != null) expressions += boolExpr

        case Defn.Val(mods, List(Pat.Var(Term.Name(name))), decltpe, rhs) =>
          val (getRHS, typTemp) = getExpr(rhs, funcVars)
          if(!solvable) return (ListBuffer(), null)

          funcVars = addFuncVars(funcVars, typTemp, name) // adding the variable to the list
          expressions += ctx.mkEq(getFuncVars(funcVars, name)._1, getRHS) // "assigning" the rhs to the created (above) constant

        case Defn.Var(mods, List(Pat.Var(Term.Name(name))), decltpe, rhs) =>

          val (getRHS, typTemp) = getExpr(rhs.get, funcVars)
          if(!solvable) return (ListBuffer(), null)
          funcVars = addFuncVars(funcVars, typTemp, name)
          expressions += ctx.mkEq(getFuncVars(funcVars, name)._1, getRHS)

        case Term.Assign(Term.Name(lhsName), rhs) =>
          val lhsExpr = getFuncVars(funcVars, lhsName)
          val rhsExpr = getExpr(rhs, funcVars)._1
          val lhsNewName = lhsName + "_" + count; incCount()
          funcVars = addFuncVars(funcVars, lhsExpr._2, lhsNewName)
          if(!solvable) return (ListBuffer(), null)
          val lhsNewExpr = getFuncVars(funcVars, lhsNewName)._1
          expressions += ctx.mkEq(lhsNewExpr, rhsExpr)

        case Term.ApplyInfix(lhs, op, targs, args) =>
          var boolExpr = getBoolExpr(Term.ApplyInfix(lhs, op, targs, args), funcVars)
          if (boolExpr != null) expressions += boolExpr

        case Term.If(condTerm, ifBlock, elseBlock) =>
          val cond : BoolExpr = getBoolExpr(condTerm, funcVars)
          temp_count = count // saving count before if block
          val (ifExpr, funcVarsTempIf) = traverseStat(ifBlock, funcVars)
          count = temp_count // resetting count to before if block
          val (elseExpr, funcVarsTempElse) = traverseStat(elseBlock, funcVars)

          // if the funcvars are the same size
          if(funcVarsTempIf.vars.keySet == funcVarsTempElse.vars.keySet) {
            val ifFullExpr = ctx.mkAnd(cond, aggregate(ifExpr))
            val elseFullExpr = ctx.mkAnd(ctx.mkNot(cond), aggregate(elseExpr))
            val iteFullExpr = ctx.mkOr(ifFullExpr, elseFullExpr)
            expressions += iteFullExpr
            funcVars = funcVarsTempElse // makes no difference which
          }
          else {
            println("DISCLAIMER")
            println("The if block and else block have different assignments. This format is currently not supported. ")
            println("Statement: " + stat)
            println("Please make sure that both blocks modify the same variables the same amount of times. ")
            count = temp_count
          }

        case Term.While(cond, Term.Block(stats)) =>
          solver.push()
          solver.push()
          var infCount = 0 // to avoid infinite loops
          val max_loops = 100
          var current = getBoolExpr(cond, funcVars)
          if(current != null) {
            solver.add(current)
            while (solver.check() == Status.SATISFIABLE && infCount < max_loops) {
              expressions += current
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
            println("If this limit is reached, it is assumed that the final iteration has negated the loop condition.")
          }
          expressions += ctx.mkNot(current) // adding final assignment negation since loop breaks so it obeys this

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
            while (solver.check() == Status.SATISFIABLE && infCount < max_loops) {
              expressions += current
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
            println("If this limit is reached, it is assumed that the final iteration has negated the loop condition.")
          }
          expressions += ctx.mkNot(current)

        case Term.Block(stats) =>
          for (stat <- stats) {
            var (exprs, funcVarsTemp) = traverseStat(stat, funcVars)
            if(!solvable) return (ListBuffer(), null)
            for (expr <- exprs) expressions += expr
          }
        case Term.Apply(Term.Name(name), args) =>
          if (utilFuncs.keySet.contains(name)) {
            isFromCond = false
            val funcExpr : BoolExpr = traverseBlock(name, args, funcVars)
            expressions += funcExpr
          }
          else {
            println("Function does not exist")
          }

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

    def traverseBlock(funcName : String, args : List[Term], funcVars : Variables) : BoolExpr = {
      val (params, stats) = utilFuncs(funcName)

      var utilParams : mutable.LinkedHashMap[String, (Expr[_], Int)] = mutable.LinkedHashMap()

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
        }
      }

      var eqArgParam : ListBuffer[BoolExpr] = ListBuffer()
      if (args.length == utilParams.size) {
        var counter = 0
        while (counter < args.length) {
          val arg = args(counter)
          val (argExpr, typTemp) = getExpr(arg, funcVars)
          if(!solvable) return null
          val param: Expr[_] = utilParams(utilParams.keys.toList(counter))._1
          eqArgParam += ctx.mkEq(argExpr, param)
          counter += 1
        }
      }
      else {
        println("NUMBER OF ARGUMENTS DON'T MATCH NUMBER OF PARAMETERS")
      }

      var newFuncVars : Variables = new Variables()
      for (param <- utilParams) {
        newFuncVars.vars = newFuncVars.vars + (param._1 -> param._2)
      }

      isFromCond = false
      var allExprsInBlock : ListBuffer[BoolExpr] = ListBuffer()
      for (stat <- stats) {
        val (exprs, _) = traverseStat(stat, newFuncVars)
        if(!solvable) return null
        for(expr <- exprs) allExprsInBlock += expr
      }

      var agg = aggregate(allExprsInBlock)
      for(eq <- eqArgParam) agg = ctx.mkAnd(eq, agg)
      agg
    }

    def traverseDefns(defns : List[Stat]) : Unit = {
      for (defn <- defns) {
        defn match {
          case Defn.Def(a, Term.Name(name), b, List(params), typ, Term.Block(stats)) =>

            utilFuncs = utilFuncs + (name -> (params, stats))

          case _ =>
            println("TERM TYPE MISMATCH : Definition")
        }
      }
    }

    def traverseObject(obj : Defn.Object) : Unit = obj match {
      case Defn.Object(mods, name, Template(a, b, c, list)) =>
        traverseDefns(list)
      case _ =>
        println("TERM TYPE MISMATCH : Object(2)")
    }

    def traverseUtil(utilTree : Tree):Unit = {
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

    }

    def traverse(conditionTree : Term):BoolExpr={
      global = true
      val conditionExpression = getBoolExpr(conditionTree, null)
      if(solvable && (conditionExpression != null)) {
        solver.add(conditionExpression)
        conditionExpression
      } else {
        println("The current condition is not solvable. We hence assume satisfiability to avoid accidental deletion")
        val litBool = getBoolExpr(Lit.Boolean(true), null)
        solver.add(litBool) // assuming satisfiable condition
        litBool
      }
    }
  }

  def generateUtilFunctions(util : String) : Unit = {

    var utilTree : Tree = null

    if (util.contains("def")) {
      utilTree = util.parse[Source].get
      traverser.traverseUtil(utilTree)
    }
    else {
      utilTree = null
    }
  }

  def generateFormulas(conditions : String, vars : Map[String, String]) : (Term, BoolExpr) = {
    for (variable <- vars) {
      variable._2 match {
        case "Int" =>
          addVars(integer, variable._1)
        case "Boolean" =>
          addVars(boolean, variable._1)
        case "String" =>
          addVars(string, variable._1)
      }
    }

    val conditionTree = conditions.parse[Term].get

    val conditionExp = traverser.traverse(conditionTree)

    (conditionTree, conditionExp)
  }

  def checkUnsat() : Boolean = {
    if(traverser.getSolver.check == Status.SATISFIABLE) {
      traverser.clearSolver()
      true
    }
    else {
      traverser.clearSolver()
      false
    }

  }

  def solveAgg(aggConds : BoolExpr) : Boolean = {
    traverser.getSolver.add(aggConds)

    if(traverser.getSolver.check == Status.SATISFIABLE) {
      traverser.clearSolver()
      true
    }
    else {
      traverser.clearSolver()
      false
    }
  }
}
