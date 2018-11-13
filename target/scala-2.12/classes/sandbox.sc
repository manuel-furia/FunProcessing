import Utils._
import collection.immutable.HashMap
import Parser._
import scala.collection.immutable.HashMap
import Parser.Expr.NumericValue


//val parser = new ExprParser(Set("x"), binaryOps = BinaryOperator.table )


//parser.evaluate("4 * ( 3 + 4 )")(Map("x" -> Left(45)))

//parser.evaluate("3i")(Map("x" -> Left(5)))



/*
val defp = new DefinitionParser

defp.define("def f(x) = x + 3").get._3(Map("x" -> Left(8)))

val defpar = new ParameterParser

defpar.define("par k from 0 to 10 step 2")

*/

abstract class Acceptable[T]

object Acceptable {
  implicit object IntOk extends Acceptable[Int]
  implicit object LongOk extends Acceptable[Long]
}

def f[T: Acceptable](t: T) = t

f(1L)


val rndGen = new scala.util.Random
def rnd = rndGen.nextDouble()

List(rnd, rnd, rnd)