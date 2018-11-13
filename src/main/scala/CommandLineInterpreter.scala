package Interpreter
import Chart.{Chart, Color, Parameters}
import MathFunction.Func.{RealParameter, UserFunc}
import Parser.DefinitionParser
import Parser.CommandParser
import Parser.ExprParser
import Utils.Complex
import java.io.PrintStream

import MathFunction.{CtoCFunc, RtoRFunc}

/**
  * Created by manuel on 7/31/17.
  */
class CommandLineInterpreter(val context: Context) {

  type Error = String

  val quit = context.quit

  private val unary = Map()
  private val binary = Map()
  private val builtin = Map()


  def execute(line: String): Either[Error, CommandLineInterpreter] = {
    try {
      Right(dispatch(line))
    }
    catch{
      case ex: Exception => Left(ex.getMessage)
    }

  }

  private def dispatch(line: String): CommandLineInterpreter = {
    if (line.startsWith("def ") || line.startsWith("let ")) {
      handleDefinition(line)
    }
    else if (CommandParser.commands.foldLeft(false){(z, e) => z || line.startsWith(e)}) {
      handleCommand(line)
    }
    else {
      handleExpression(line)
    }

  }

  private def handleDefinition(line: String): CommandLineInterpreter = {
    val definitionParser = new DefinitionParser(context.constants, Map(), Map(), Map(), context.userFuncs, context.realParameters)
    val newContext = definitionParser.define(line).foldLeft(context)((S, x) => x match {
      case Left(func) => S.copy(userFuncs = Map(func._1 -> func) ++ S.userFuncs)
      case Right(Left(consts)) => S.copy(constants = consts ++ S.constants)
      case Right(Right(realParam)) => S.copy(realParameters = Seq(realParam) ++ S.realParameters)
    })


    new CommandLineInterpreter(newContext)
  }

  private def handleCommand(line: String): CommandLineInterpreter = {
    val commandParser = new CommandParser()
    val command = commandParser.readCommand(line)

    def rnd = scala.util.Random.nextFloat

    command match {
      case ("quit", lst) => {
        if (context.debug.nonEmpty)
          context.debug.get.print(lst.mkString(" "))
        new CommandLineInterpreter(context.copy(quit=true))
      }
      case ("plot", List("all")) => {
        val charts = context.userFuncs.map(f => new Chart(new RtoRFunc(f._2) , Parameters(foreColor = Color(rnd, rnd, rnd)))).toList
        val g = new Graphics.Graphics(charts)
        g.run()
        this
      }
      case ("plot", List("all", "complex")) => {
        val charts = context.userFuncs.map(f => new Chart(new CtoCFunc(f._2) , Parameters(foreColor = Color(rnd, rnd, rnd)))).toList
        val g = new Graphics.Graphics(charts)
        g.run()
        this
      }
      case ("clear", lst) => {
        new CommandLineInterpreter(Context.baseConsole)
      }
      case (com, _) => throw new Exception("Error: Command " + com + " not found.")
    }

  }

  private def handleExpression(line: String): CommandLineInterpreter = {
    import scala.util.Try
    val exprParser = new ExprParser(variables = Set(), context.constants, Map(), Map(), Map(), context.userFuncs)
    val resConstIndices = context.constants.keys.filter(_.startsWith("res")).map(_.drop(3)).flatMap(x => Try(x.toInt).toOption)
    val maxResConstIndex = Try(resConstIndices.max).toOption
    val newResConstName = maxResConstIndex match {
      case None => "res0"
      case Some(x) => "res" + (x+1).toString
    }
    val result = exprParser.evaluate(line)(Map())
    val newResParameter = Map(newResConstName -> result)

    if (context.debug.nonEmpty)
      context.debug.get.print(newResConstName + ": " + result.toString)

    new CommandLineInterpreter(context.copy(debug = context.debug, constants = newResParameter ++ context.constants))
  }

}

object Context{
  val baseConsole = Context(Nil, Map(), Map(), Seq(), Some(Console.out))
  val base = Context(Nil, Map(), Map(), Seq(), None)

}

case class Context(charts: List[Chart[Any, Any]], userFuncs: Map[String, UserFunc], constants: Map[String, Complex], realParameters: Seq[(String, Double, Double, Double)] = Seq(), debug: Option[PrintStream] = None, quit: Boolean = false)