import Chart._
import Graphics._
import Parser.{BinaryOperator, DefinitionParser, ExprParser}
import Utils.Complex
import Profile.TestComputationSpeed
import Interpreter.{CommandLineInterpreter, Context}

object Main {
  def main(args: Array[String]) {



    //Timing function evaluation:
    /*
    val funcParser = new DefinitionParser()
    val func = new MathFunction.RtoRFunc( funcParser.define("def f[x] = e^x + (x^2 * x) + 2").left.get )
    val func2 = new MathFunction.CtoCFunc ( funcParser.define("def f[x] = e^(3i + x + 5i x)").left.get )

    println(TestComputationSpeed.functionExecTime(func, start = -1000.0, increment = 0.1, n = 2000000, parallel = true))
    println(TestComputationSpeed.functionExecTime(func2, start = -1000.0, increment = 0.1, n = 2000000, parallel = true))
    println(TestComputationSpeed.functionExecTime(func, start = -1000.0, increment = 0.1, n = 2000000))
    println(TestComputationSpeed.functionExecTime(func2, start = -1000.0, increment = 0.1, n = 2000000))
    */

    /*Results (in ms):
    665.68805
    732.369582
    1856.372321
    2593.2453
    */

/*
    val funcParser = new DefinitionParser()
    val func = new MathFunction.RtoRFunc( funcParser.define("def f[x] = x").head.left.get )
    //val cmplxfunc = new MathFunction.CtoCFunc(funcParser.define("def f[z] = 10sin[z]").left.get)


    val sketch = new Graphics(List(Chart(func, Parameters())))

    sketch.run()

*/
    val interpreter = new CommandLineInterpreter(Context.baseConsole)

    Stream.from(0).scanLeft(interpreter) {(currentInterpreter, i) =>
      currentInterpreter.execute(scala.io.StdIn.readLine("%s", "\n>> ")) match {
        case Right(inter) => inter
        case Left(err) => {println("Error: " + err); currentInterpreter}
      }
    }.takeWhile(i => !i.quit).toList

    interpreter.execute(scala.io.StdIn.readLine)



    //val sketch = new Graphics(List(Chart(cmplxfunc, Parameters())))

    //sketch.run()

  }
}