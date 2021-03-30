package monitor.parser

//import smtlib.parser.ParserCommands
import smtlib.lexer.Tokens.OParen
import smtlib.parser.ParserTerms
import smtlib.trees.Commands._
//import smtlib.theories.Core.{Not, Or}
//import smtlib.trees.Commands.Command

//import java.io._
import scala.collection.mutable.ListBuffer
import scala.sys.process._
//import smtlib.parser.Commands._
//import scala.tools.nsc.transform.patmat.Solving
//import monitor.model.CNF
//package z3.scala

import java.io._

import smtlib.trees.Terms._

import smtlib.theories.Core._

import scala.sys.process._

//import z3

class STSolverZ3 {

  private lazy val z3_path = {
    sys.env.getOrElse("Z3_EXE", "z3.exe")
  }

  // find way to eliminate conditions that have nothing to do with the unsatisfiability - saveUnsatConds()
  private var lemmas : List[String] = List() // SMT read this and see if it is the same semantics
  //private val cnfTransformer = new TransformToCnf()

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

  def generateSMTLIBString (condition : String) : String = {
    // use toolbox to parse through command
    // use case matching to traverse the created tree recursively
    // in each case write the corresponding smtlib format command

    var smtlibConditions =
      """
        |(declare-const x Int)
        |(declare-const y Int)
        |(declare-const z Int)
        |(declare-const a1 (Array Int Int))
        |(declare-const a2 (Array Int Int))
        |(declare-const a3 (Array Int Int))
        |(assert (= (select a1 x) x))
        |(assert (= (store a1 x y) a1))
        |(assert (not (= x y)))
        |(check-sat)
        |""".stripMargin

    smtlibConditions
  }

  def convertConditionsToSMTLIB(conditions : String) : Stream[Command] = { // change conditions to list to separate conditions of different labels


    // I CANT USE TERM

    var smtlibConditions = generateSMTLIBString(conditions)
    println("``` SMTLIB COMMANDS ```")
    println(smtlibConditions)

    val is = new java.io.StringReader(smtlibConditions)
    val lexer = new smtlib.lexer.Lexer(is)
    val parser = new smtlib.parser.Parser(lexer)

    println("Reading script...")

    val script: List[Command] = {
      var cmds = new ListBuffer[Command]
      var cmd : Command = parser.parseCommand //SetOption(ProduceModels(true))  //parser.parseCommand
      println("Command: " + cmd)
      while(cmd != null) {
        cmds.append(cmd)
        cmd = parser.parseCommand
        println("Command: " + cmd)
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

  def processConditions(conditions : String) : Stream[Command] = { // ParserTerms
    // do the things to read the smtlib file and change them to term NO TO COMMANDS
    // OR
    // there s an smtlib command that makes z3 check sat (see z3 easy tutorial) <-- TRY THIS FIRST
    // ^ this actually doesn't work nevermind


    val script = convertConditionsToSMTLIB(conditions)

    script
  }
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


  private def processInput(aggConds: String): (OutputStream => Unit) = {
    (stdin: OutputStream) => {
      val printer = new PrintWriter(new BufferedOutputStream(stdin))
      processConditions(aggConds).foreach(
        (cmd) => {
          printer.write(cmd.toString())
        }
      )
      printer.close()
    }
  }

  def checkUnsat(aggConds : String) : String = { // change this to accept formula
    // return nothing if satisfiable
    // save current condition
    // for each combination of previous conditions
    //    check satisfiability with last condition
    //    return if the unsat part found, dont continue loop to find more complex combinations
    // start with last cond and 1 other cond
    // keep adding combinations from there

    // SOMEHOW MAKE THIS STUFF BELOW WORK LIKE STUFF ABOVE
    // save aggregated condition as a list instead of a string so that they can be handled separately
    //val formula = True() // processConditions(aggConds)
    //val

    val process = Process(z3_path, Seq("-smt2", "-in"))
    var result: String = null
    val processIO = new ProcessIO(
      processInput(aggConds),
      stdout => {
        val reader = new BufferedReader(new InputStreamReader(stdout))
        result = reader.readLine()
      },
      stderr => {
        scala.io.Source.fromInputStream(stderr)
          .getLines.foreach(println)
      }
    )
    val handle = process.run(processIO)
    assert(handle.exitValue() == 0)
    assert(result == "sat" || result == "unsat")
    if (result == "sat") {
      "SAT ~~~~~~~~~~~~~~~~~~~" // Some(Map())
    } else {
      "UNSAT ~~~~~~~~~~~~~~~~~~" // None
    }


    //null
  }

}
