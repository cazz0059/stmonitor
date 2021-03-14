package monitor.parser

import scala.collection.mutable
import scala.reflect.runtime._
import scala.reflect.runtime.universe._
import scala.tools.reflect.ToolBox

import monitor.model._
import monitor.model.Scope

class STSolverHelper {

  ////////////////////////////////////////////

  def notFactorToString(not_factor : NotFactor): String = {
    var stringCondition = ""

    if (not_factor.t) {
      stringCondition = stringCondition ++ " not"
    }
    stringCondition = stringCondition ++ " "
    not_factor.factor match {
      case Expression(terms) =>
        stringCondition = stringCondition ++ conditionToString(Expression(terms))
      case Variable(name) =>
        stringCondition = stringCondition ++ name
    }

    stringCondition
  }

  def termToString(term : Term): String = {
    var stringCondition = ""

    val not_factors = term.not_factors
    if (not_factors.length > 1) {
      for (not_factor <- not_factors) {
        stringCondition = stringCondition ++ "("
        stringCondition = stringCondition ++ notFactorToString(not_factor)
        stringCondition = stringCondition ++ ") && "
      }
      stringCondition.dropRight(3) // removing last and
    }
    else if (not_factors.length == 1){
      stringCondition = stringCondition ++ notFactorToString(not_factors.head)
    }
    stringCondition
  }

  def conditionToString(expression : Expression) : String ={
    var stringCondition = ""
    val terms = expression.terms
    if (terms.length > 1) {
      for (term <- terms) {
        stringCondition = stringCondition ++ "("
        stringCondition = stringCondition ++ termToString(term)
        stringCondition = stringCondition ++ ") || "
      }
      stringCondition.dropRight(3) // removing last or
    }
    else if (terms.length == 1) {
      stringCondition = stringCondition ++ termToString(terms.head)
    }

    stringCondition
  }

  ////////////////////////////////////////////  CNF BUILDERS

  def negate(not_factor : NotFactor) : NotFactor = {
    //    not_factor.factor match {
    //      case Variable(name)
    //    }
    NotFactor(!not_factor.t, not_factor.factor)
  }

  def negate(term : Term): Term = {
    if (term.not_factors.length == 1) {
      val not_factor = NotFactor(!term.not_factors.head.t, term.not_factors.head.factor)
      Term(List(not_factor))
    }
    val not_factor = NotFactor(t = true, term.not_factors.head.factor)
    Term(List(not_factor))
  }

  def negate(expression : Expression) : Expression = { // this is never called, is it because it is not needed
    // or am I missing something?
    if (expression.terms.length == 1) {
      if (expression.terms.head.not_factors.length == 1) {
        val not_factor = NotFactor(!expression.terms.head.not_factors.head.t, expression.terms.head.not_factors.head.factor)
        val term = Term(List(not_factor))
        Expression(List(term)) // depends if the factor is negated or not
      }
    }
    val not_factor = NotFactor(t = true, expression.terms.head.not_factors.head.factor)
    val term = Term(List(not_factor))
    Expression(List(term)) // normal negation
  }

  def mergeCNF(cnf1 : Set[Set[(String, Boolean)]], cnf2 : Set[Set[(String, Boolean)]]) : Set[Set[(String, Boolean)]] = {
    var cnf : Set[Set[(String, Boolean)]] = Set()
    for (x <- cnf1.toIterator) {
      for (y <- cnf2.toIterator) {
        val clause = Set(x, y)
        cnf = cnf + clause
      }
    }

    cnf
  }

  //  private def getCurrentConditions(factor : Factor) : Set[Set[(String, Boolean)]] = {
  //    factor match {
  //      case Expression(terms) =>
  //        getCurrentConditions(Expression(terms))
  //      case Variable(name) =>
  //        Set(name) // base case, change to if negation or not
  //    }
  //  }

  def getCurrentConditions(not_factor : NotFactor) : Set[Set[(String, Boolean)]] = {
    var cnf : Set[Set[(String, Boolean)]] = Set()
    if (not_factor.t) { // 'not' present
      not_factor.factor match {
        case Expression(terms) =>
          if (terms.length > 1) {
            for (term <- terms) {
              cnf = cnf ++ getCurrentConditions(negate(term))
            }
          }
          else { // cannot be empty
            val term = terms.head
            if(term.not_factors.length > 1) {
              for (not_factor <- term.not_factors) {
                cnf = mergeCNF(cnf, getCurrentConditions(negate(not_factor)))
              }
            }
            else { // cannot be empty
              cnf = cnf ++ getCurrentConditions(negate(term.not_factors.head))
            }
          }
        case Variable(name) =>
          val literal = (name, true)
          cnf = cnf ++ Set(Set(literal))
      }
      cnf
    }
    else { // 'not' not present
      not_factor.factor match {
        case Expression(terms) =>
          getCurrentConditions(Expression(terms))
        case Variable(name) =>
          val literal = (name, false)
          Set(Set(literal)) // base case
        //Set[Set[(String, Boolean)]]() + ((name, true)) // base case, change to if negation or not
      }
    }
  }

  def getCurrentConditions(term : Term) : Set[Set[(String, Boolean)]] = {
    val not_factors = term.not_factors
    var cnf : Set[Set[(String, Boolean)]] = Set()
    for (not_factor <- not_factors) {
      cnf = cnf ++ getCurrentConditions(not_factor)
    }
    cnf
  }

  def getCurrentConditions(expression : Expression) : Set[Set[(String, Boolean)]] = {
    val terms = expression.terms
    var cnf : Set[Set[(String, Boolean)]] = Set()
    for (term <- terms) {
      cnf = mergeCNF(cnf, getCurrentConditions(term))
    }
    cnf
  }

}
