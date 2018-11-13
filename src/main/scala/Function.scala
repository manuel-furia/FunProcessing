package MathFunction

import Utils.Complex
import Parser.Expr.UserFunc
import Parser.Expr.Argument
import Parser.Expr.Formula

object Func{
  type RealParameter = Parser.Expr.RealParameter
  type UserFunc = Parser.Expr.UserFunc

  object Empty extends Func[Any, Any]{
    override def apply(arg: Any): Any = Complex.NaN
  }
}

trait Func[-T, +O] {
  def apply(arg: T): O
}

trait GenericFunction extends Func[Any, Any]

trait ScalarFunc {
  val parsed: UserFunc

  def applyHelper(arg: (String, Complex)): Complex = {
    val (variable, value) = arg
    val argument = Map(variable -> value)
    val formula = parsed._3.dropWhile(case_ => case_._2(argument).isZero).headOption
    formula match {
      case Some((value, cond)) => value(argument)
      case None => Complex.NaN
    }
  }

}

trait VectorFunction {
  val parsedComponents: Vector[UserFunc]

  def applyHelper(arg: (Vector[String], Vector[Complex])): Vector[Complex] = {
    val (variables, vector) = arg
    val argument = variables.zip(vector).toMap
    parsedComponents.map { component =>
      val formula = component._3.dropWhile(case_ => case_._2(argument).isZero).headOption
      formula match {
        case Some((value, cond)) => value(argument)
        case None => Complex.NaN
      }
    }
  }
}

class RtoRFunc(val parsed: UserFunc) extends Func[(String, Double), Double] with ScalarFunc {
  override def apply(arg: (String, Double)): Double = applyHelper((arg._1, Complex(arg._2))).re
}

class CtoRFunc(val parsed: UserFunc) extends Func[(String, Complex), Double] with ScalarFunc  {
  override def apply(arg: (String, Complex)): Double = applyHelper(arg).re
}

class RtoCFunc(val parsed: UserFunc) extends Func[(String, Double), Complex] with ScalarFunc  {
  override def apply(arg: (String, Double)): Complex = applyHelper((arg._1, Complex(arg._2)))
}

class CtoCFunc(val parsed: UserFunc) extends Func[(String, Complex), Complex] with ScalarFunc  {
  override def apply(arg: (String, Complex)): Complex = applyHelper(arg)
}

class R2toR2Func(parsedX: UserFunc, parsedY: UserFunc) extends Func[((String, String), (Double, Double)), (Double, Double)] with VectorFunction {
  override val parsedComponents = Vector(parsedX, parsedY)
  override def apply(arg: ((String, String), (Double, Double))): (Double, Double) = {
    def tuplePair2TupleVector[T, Q](t: ((T, T), (Q, Q))): (Vector[T], Vector[Q]) = (Vector(t._1._1, t._1._2), Vector(t._2._1, t._2._2))
    def complexify(arg: (Vector[String], Vector[Double])): (Vector[String], Vector[Complex]) = (arg._1, arg._2.map(x => Complex(x)))
    val res = applyHelper(complexify(tuplePair2TupleVector(arg)))
    (res(0).re, res(1).re)
  }
}

class RtoR2Func(parsedX: UserFunc, parsedY: UserFunc) extends Func[((String, String), (Double)), (Double, Double)] with VectorFunction {
  override val parsedComponents = Vector(parsedX, parsedY)
  override def apply(arg: ((String, String), (Double))): (Double, Double) = {
    def tuplePair2TupleVector[T, Q](t: ((T, T), Q)): (Vector[T], Vector[Q]) = (Vector(t._1._1, t._1._2), Vector(t._2))
    def complexify(arg: (Vector[String], Vector[Double])): (Vector[String], Vector[Complex]) = (arg._1, arg._2.map(x => Complex(x)))
    val res = applyHelper(complexify(tuplePair2TupleVector(arg)))
    (res(0).re, res(1).re)
  }
}