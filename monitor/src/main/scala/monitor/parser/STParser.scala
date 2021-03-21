package monitor.parser

import monitor.model._

import scala.language.postfixOps
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class STParser extends StandardTokenParsers {
  lexical.reserved += ("rec", "end", "String", "Int", "Boolean") //, "and", "or", "not"

  lexical.delimiters += ("?", "!", "&", "+", "(", ")", "{", "}", ",", ":", "=", ".", "[", "]") // , "|"

  private var sendChoiceCounter: Int = 0
  private var receiveChoiceCounter: Int = 0

  def sessionTypeVar: Parser[SessionType] = (ident <~ "=") ~ sessionType ^^ {
    case i ~ t =>
      new SessionType(i, t)
  }

  def sessionType: Parser[Statement] = positioned (choice | receive | send | recursive | recursiveVar | end) ^^ {a=>a}

  def choice: Parser[Statement] = positioned( receiveChoice | sendChoice ) ^^ {a=>a}

  def receive: Parser[ReceiveStatement] = ("?" ~> ident) ~ ("(" ~> types <~ ")") ~ opt("[" ~> stringLit <~ "]") ~ opt("." ~> sessionType) ^^ {
    case l ~ t ~ None ~ None =>
      println(l)
      ReceiveStatement(l, t, null, End()) // Expression(Nil)
    case l ~ t ~ None ~ cT =>
      println(l)
      ReceiveStatement(l, t, null, cT.get)
    case l ~ t ~ c ~ None =>
      println(l)
      ReceiveStatement(l, t, c.get, End())
    case l ~ t ~ c ~ cT =>
      println(l)
      println("Hello")
      ReceiveStatement(l, t, c.get, cT.get)
  }

  def receiveChoice: Parser[ReceiveChoiceStatement] = "&" ~ "{" ~> (repsep(sessionType, ",") <~ "}") ^^ {
    cN =>
      for (s <- cN) {
        println("Receive Choice")
        s match {
          case _: ReceiveStatement =>
          case _ =>
            throw new Exception("& must be followed with ?")
        }
      }
      ReceiveChoiceStatement(f"ExternalChoice${receiveChoiceCounter+=1;receiveChoiceCounter.toString}", cN)
  }

  def send: Parser[SendStatement] = ("!" ~> ident) ~ ("(" ~> types <~ ")") ~ opt("[" ~> stringLit <~ "]") ~ opt("." ~> sessionType) ^^ {
    case l ~ t ~ None ~ None =>
      println(l)
      SendStatement(l, t, null, End()) // Expression(Nil)
    case l ~ t ~ None ~ cT =>
      println(l)
      SendStatement(l, t, null, cT.get)
    case l ~ t ~ c ~ None =>
      println(l)
      SendStatement(l, t, c.get, End())
    case l ~ t ~ c ~ cT =>
      println(l)
      SendStatement(l, t, c.get, cT.get)
  }

  def sendChoice: Parser[SendChoiceStatement] = "+" ~ "{" ~> (repsep(sessionType, ",") <~ "}") ^^ {
    cN =>
      for (s <- cN) {
        println("Send Choice")
        s match {
          case _: SendStatement =>
          case _ =>
            throw new Exception("+ must be followed with !")
        }
      }
      SendChoiceStatement(f"InternalChoice${sendChoiceCounter+=1;sendChoiceCounter.toString}", cN)
  }

  def recursive: Parser[RecursiveStatement] = ("rec" ~> ident <~ ".") ~ ("(" ~> sessionType <~ ")") ^^ {
    case i ~ cT =>
      RecursiveStatement(i, cT)
  }

  def recursiveVar: Parser[RecursiveVar] = ident ~ opt("." ~> sessionType) ^^ {
    case i ~ None =>
      RecursiveVar(i, End())
    case i ~ cT =>
      RecursiveVar(i, cT.get)
  }

//  // https://gist.github.com/sofoklis/3343973 used for parsing conditions
//
//  def conditions: Parser[Expression] = (term ~ rep("or" ~> term)) ^^ { // rep1sep(term, "or")
//    case t ~ Nil =>
//      Expression(List(t))
//    case t ~ terms => // Some("or" ~ c) =>
//      Expression(List(t) ++ terms)
////    terms =>
////      for (t <- terms) {
////        t match {
////          case _: Term =>
////          case _ =>
////            throw new Exception("Not a term!")
////        }
////      }
////      Expression(terms)
//  }
//
//  def term: Parser[Term] = (not_factor ~ rep("and" ~> not_factor)) ^^ { // rep1sep(not_factor, "and")
//    case nf ~ Nil =>
//      Term(List(nf))
//    case nf ~ nfs => // Some("and" ~ t) =>
//      Term(List(nf) ++ nfs)
////    not_factors =>
////      for (f <- not_factors) {
////        f match {
////          case _: NotFactor =>
////          case _ =>
////            throw new Exception("Not a factor!")
////        }
////      }
////      Term(not_factors)
//  }
//
//  def not_factor: Parser[NotFactor] = (opt("not") ~ factor) ^^ {
//    case Some("not") ~ f =>
//      NotFactor(t = true, f);
//    case None ~ f =>
//      NotFactor(t = false, f)
//  }
//
//  def variable: Parser[Variable] = stringLit ^^ {
//    v =>
//      Variable(v)
//  }
//
//  def factor: Parser[Factor] = "(" ~> conditions <~ ")" | variable^^ { // positioned(variable | "(" ~> conditions <~ ")") ^^ {exp=>exp}
//    exp => exp
//  }

  def types: Parser[Map[String, String]] = repsep(typDef, ",") ^^ {
    _ toMap
  }

  def typDef: Parser[(String, String)] = (ident <~ ":") ~ typ ^^ {
    case a ~ b =>
      (a, b)
  }

  def end: Parser[End] = ("" | "end") ^^ (_ => End())

  def typ: Parser[String] = "String" | "Int" | "Boolean" ^^ (t => t)

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] = {
    val assertionPattern = """\[(.*?)\]""".r
    phrase(p)(new lexical.Scanner(assertionPattern.replaceAllIn(in, "[\""+_.group(1).replace("\"", "\\\\\"")+"\"]")))
  }
}