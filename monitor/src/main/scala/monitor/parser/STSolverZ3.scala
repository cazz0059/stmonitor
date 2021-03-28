package monitor.parser

import scala.tools.nsc.transform.patmat.Solving
//import monitor.model.CNF
//package z3.scala

//import z3

class STSolverZ3 {

  // find way to eliminate conditions that have nothing to do with the unsatisfiability - saveUnsatConds()
  private var lemmas : List[String] = List() // SMT read this and see if it is the same semantics
  //private val cnfTransformer = new TransformToCnf()

  def addLemma(payload : String): Unit = {
    lemmas = payload :: lemmas
  }

  def compareToLemmas(payload : String): Unit = {
    // can be done with toolbox - no use cnf instead, I think it saves easier

    // get semantics of payload - cnf clauses
    // for each lemma
    //    get semantics of lemma
    //    compare semantics - check if clauses in lemma make up subset of clauses in payload


  }

//  def saveUnsatConds(unsatCNF : CNF) : CNF = {
//    // for each combination of clauses
//    //    check which part of that clause is unsat (using z3), and return that one
//    //    if found the unsat part, no need to proceed with the loop
//    // start with the smallest combinations - optimisation
//    // include always the last condition/clause added as part of the unsat - optimisation
//    unsatCNF
//  }

  def checkUnsat(payload : String) : String = {
    // return nothing if satisfiable
    // save current condition
    // for each combination of previous conditions
    //    check satisfiability with last condition
    //    return if the unsat part found, dont continue loop to find more complex combinations
    // start with last cond and 1 other cond
    // keep adding combinations from there
    null
  }

}
