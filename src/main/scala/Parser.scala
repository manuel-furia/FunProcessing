package Parser
/**
  * Created by manuel on 7/11/17.
  */
import Utils._
import sun.security.pkcs.ParsingException

import scala.math._
import scala.util.parsing.combinator._
import scala.util.Random
import scala.collection.immutable.Range

object Expr {
  type Argument = Map[String, Complex]
  type Formula = Argument => Complex
  type UserFunc  = (String, List[String], List[(Formula, Formula)])
  type Constants = Map[String, Complex]
  type RealParameter = (String, Double, Double, Double)
  type Command = (String, List[String])
}

import Expr._

class ExprParser(val variables: Set[String] = Set(),
                 val constants: Constants = Map(),
                 val unary: Map[String, Complex => Complex] = Map(),
                 val binaryOps: Map[(String, Int), (Complex, Complex) => Complex] = Map(),
                 val builtinFuncs: Map[String, (List[Complex]) => Complex] = Map(),
                 val userFcts: Map[String, UserFunc] = Map(),
                 random: Random = new Random) extends JavaTokenParsers with RegexParsers {


  require(constants.keySet.intersect(userFcts.keySet).isEmpty)

  private val allConstants = constants ++ Map("i" -> Complex.i, "e" -> Complex(E), "E" -> Complex(E), "PI" -> Complex(Pi), "Pi" -> Complex(Pi), "pi" -> Complex(Pi))
  private val pAllConstants = map2Parser(allConstants)

  private val allVars = variables
  private val pAllVars = set2Parser(allVars)

  private val unaryOps = unary ++ UnaryOperator.table
  private val pUnaryOps = map2Parser(unaryOps)

  private val allBinaryOpsWithPrecedence = binaryOps ++ BinaryOperator.table
  private val binaryOps_groupExp = allBinaryOpsWithPrecedence.filter(_._1._2 == 0).map(x => x._1._1 -> x._2)
  private val binaryOps_groupMult = allBinaryOpsWithPrecedence.filter(_._1._2 == 1).map(x => x._1._1 -> x._2)
  private val binaryOps_groupAdd = allBinaryOpsWithPrecedence.filter(_._1._2 == 2).map(x => x._1._1 -> x._2)
  private val allBinaryOpsWithoutPrecedence = allBinaryOpsWithPrecedence.map(x => x._1._1 -> x._2)
  private val pBinaryOps_groupExp = map2Parser(binaryOps_groupExp)
  private val pBinaryOps_groupMult = map2Parser(binaryOps_groupMult)
  private val pBinaryOps_groupAdd = map2Parser(binaryOps_groupAdd)

  private val allBuiltinFunctions = builtinFuncs ++ BuiltinFunction.table
  private val pBuiltinFunctions = map2Parser(allBuiltinFunctions)

  private val userFunctions = userFcts
  private val pUserFunctions = map2Parser(userFunctions)

  private def fold(d: Formula, l: List[(String, Formula)]) = l.foldLeft(d) {
    case (d1, (op, d2)) => arg => allBinaryOpsWithoutPrecedence(op)(d1(arg), d2(arg))
  }
  private def buildArgList(elem: Formula, l: List[Formula]) = elem :: l

  private def set2Parser[V](s: Set[String]) = if (s.isEmpty) failure("") else s.map(_ ^^ identity).reduceLeft(_ | _)
  private def map2Parser[V](m: Map[String, V]) = if (m.isEmpty) failure("") else m.keys.toList.map(_ ^^ identity).reduceLeft(_ | _)

  //private def expression: Parser[List[Formula]] = (args ~ rep("," ~ args)) ^^ {case el ~ l => buildList(el, l.map(x => (x._2)))}

  private def expression: Parser[Formula] = term ~ rep(pBinaryOps_groupAdd ~ term) ^^ {case b ~ l => fold(arg => b(arg), l.map(x => (x._1, x._2)))}

  private def term: Parser[Formula] = factor ~ rep(pBinaryOps_groupMult ~ factor) ^^ {
    case f ~ l => fold(arg => f(arg), l.map(x => (x._1, x._2)))
  }
  private def factor: Parser[Formula] = signed ~ rep(pBinaryOps_groupExp ~ signed) ^^ {case b ~ l => fold(arg => b(arg), l.map(x => (x._1, x._2)))}

  private def signed: Parser[Formula] = opt(pUnaryOps) ~ implMult ^^ {
    case Some(op) ~ t => arg => unaryOps(op)(t(arg))
    case None ~ t => arg => t(arg)
    case _ => throw new Exception("This should not happen, fix Parser.signed")
  }

  private def implMult: Parser[Formula] = base ~ opt(implMult) ^^ {case t ~ None => t; case t ~ Some(p) => arg => allBinaryOpsWithoutPrecedence("*")(t(arg), p(arg))}
  private def base: Parser[Formula] = simpleBrackets | literal | variable | const | userFunction | builtinFunction | rnd | rndn
  private def literal: Parser[Formula] = fpn
  private def const: Parser[Formula] = pAllConstants ^^ (name => arg => allConstants(name))
  private def variable: Parser[Formula] = pAllVars ^^ (name => arg => arg(name))
  private def rnd: Parser[Formula] = """\brnd\b""".r ^^ { case _ => (arg: Argument) => Complex(random.nextDouble)}
  private def rndn: Parser[Formula] = """\brndn\b""".r ^^ { case _ => (arg: Argument) => Complex(2 * (random.nextDouble-0.5))}

  private def simpleBrackets: Parser[Formula]  = "(" ~> expression <~ ")"

  private def userFunction: Parser[Formula] = pUserFunctions ~ "[" ~ expression ~ rep("," ~ expression) <~ "]" ^^ {
    case name ~ _ ~ firstArg ~ l => {(arg: Argument) =>
      userFunctions.get(name) match {
        case None => throw new NoSuchElementException
        case Some(f) => {
          val argValues = firstArg(arg) :: l.map(x => x._2(arg))
          val argNames = f._2
          val funcCases = f._3
          val args = argNames.zip(argValues).toMap
          val validCase = funcCases.dropWhile(_._2(args).isZero).headOption
          validCase match {
            case None => Complex.NaN
            case Some(func) => func._1(args)
          }
        }
      }
    }
  }

  private def builtinFunction: Parser[Formula] = pBuiltinFunctions ~ "[" ~ expression ~ rep("," ~ expression) <~ "]" ^^ {
    case name ~ _ ~ firstArg ~ l => {(arg: Argument) =>
      allBuiltinFunctions.get(name) match {
        case None => throw new NoSuchElementException
        case Some(f) => {
          val argValues = firstArg(arg) :: l.map(x => x._2(arg))
          val funcBody = f
          funcBody(argValues)
        }
      }
    }
  }

  //Don't consider the "-" in the beginning, it will be handled by sign
  override def floatingPointNumber: Parser[String] = """(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r

  private def fpn: Parser[Formula] =  floatingPointNumber ^^ {x => arg => Complex(x.toDouble)}

  //The complex numbers are now handles by the constant i
  /*private def cmplx: Parser[Formula] = """((\d+)?(\.\d+)?)i\b""".r ^^ {
    case "i" => (arg: Argument) => Some(Right(Complex.i))
    case img => (arg: Argument) => Some(Right(Complex(0, img.replace("i", "").toDouble)))
  }*/

  def evaluate(formula: String): Formula = parseAll(expression, formula).get
}

class DefinitionParser(val constants: Constants = Map(),
                       val unary: Map[String, Complex => Complex] = Map(),
                       val binaryOps: Map[(String, Int), (Complex, Complex) => Complex] = Map(),
                       val builtinFuncs: Map[String, (List[Complex]) => Complex] = Map(),
                       val userFcts: Map[String, UserFunc] = Map(),
                       val realPars: Seq[RealParameter] = Seq()) extends JavaTokenParsers {

  type UserFuncDef = (String, List[String])


  private def rootDef: Parser[Any] = (fundef | varPar | simplePar)

  private def varPar: Parser[RealParameter] = {
    "let" ~ ident ~ "from" ~ floatingPointNumber ~ "to" ~ floatingPointNumber ~ "step" ~ floatingPointNumber ^^ {
      case _ ~ name ~ _ ~ start ~ _ ~ end ~ _ ~ step => (name, start.toDouble, end.toDouble, step.toDouble)
      case _ => throw new ParsingException()
    }
  }

  private def simplePar: Parser[String] = {
    "let" ~ ident ^^ {
      case _ ~ name => name
      case _ => throw new ParsingException()
    }
  }

  private def fundef: Parser[UserFuncDef] = {
    "def" ~ ident ~ opt("[" ~> rep(ident ~ opt(",")) <~ "]")  ^^ {
      case _ ~ name ~ Some(args) => (name, args.map(_._1))
      case _ ~ name ~ None => (name, Nil)
      case _ => throw new ParsingException()
    }
  }

  def expandParameter(definition: String, par: RealParameter): Seq[String] = {
    val name = "{" + par._1 + "}"
    if (definition.contains(name))
      for (x <- Range.Double(par._2, par._3, par._4)) yield definition.replace(name, x.toString)
    else
      Seq()
  }

  def define(definition: String, postfix: String = ""): Seq[Either[UserFunc, Either[Constants, RealParameter]]] = {

    //Check for real parameters to expand
    val defs =  realPars.flatMap(p => expandParameter(definition, p))

    val left: Option[String] = definition.split("=").headOption
    val right: Option[String] = {
      val elems = definition.split("=")
      if (elems.length > 1) Some(elems.drop(1).mkString("="))
      else None
    }

    def defobj = (left, right) match {
      case (None, _) => throw new ParsingException()
      case (Some(l), r) => {
        parseAll(rootDef, l).get match {
          case leftResult : UserFuncDef => {
            val variables = leftResult._2.toSet
            val exprParser = new ExprParser(variables, constants, unary, binaryOps, builtinFuncs, userFcts)
            if (variables.isEmpty)
              Right(Left(Map(leftResult._1 + postfix -> exprParser.evaluate(r.getOrElse(throw new ParsingException()))(Map()))))
            else {
              val definitions = right.getOrElse(throw new ParsingException()).split(";").map(_.split("if"))
              val values = try {
                definitions.map {
                   case Array(body) => (exprParser.evaluate(body), {arg: Argument => Complex.one} : Formula)
                   case Array(body, cond) => (exprParser.evaluate(body), exprParser.evaluate(cond))
                }.toList
              }
              catch{
                case _ => throw new ParsingException()
              }
              Left((leftResult._1 + postfix, leftResult._2, values))
            }
          }
          case leftResult : RealParameter => {
            Right(leftResult match {
              case x if x._4 == 0.0 => {
                Left(Map(x._1 + postfix -> Complex(x._2)))
              }
              case x => Right(x)
            })
          }
          case leftResult : String => {
            val exprParser = new ExprParser(variables = Set(), constants, unary, binaryOps, builtinFuncs, userFcts)
            val value = exprParser.evaluate(r.getOrElse(throw new ParsingException()))(Map()).re
            Right(Left(Map(leftResult + postfix -> Complex(value))))
          }
        }

      }
    }

    if (defs.isEmpty)
      Seq(defobj)
    else
      defs.zipWithIndex.flatMap(d => define(d._1, "__" + d._2.toString()))

  }
}

object CommandParser{
  val commands = Set("plot", "title", "legend", "quit", "clear")
}

class CommandParser() extends JavaTokenParsers {

  private val pcommands = set2Parser(CommandParser.commands)

  private def set2Parser[V](s: Set[String]) = if (s.isEmpty) failure("") else s.map(_ ^^ identity).reduceLeft(_ | _)

  private def command: Parser[Command] = {
    pcommands ~ rep(ident | stringLiteral) ^^ {
      case name ~ lst => (name, lst)
      case _ => throw new ParsingException()
    }
  }


  def readCommand(parDefinition: String): Command = parseAll(command, parDefinition).get

}

