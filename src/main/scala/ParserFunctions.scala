package Parser
import scala.collection.immutable.HashMap
import Utils.Complex


object UnaryOperator {
  def unary_+(n: Complex) : Complex = n

  def unary_-(n: Complex) : Complex= n.unary_-

  def unary_~(n: Complex) : Complex  = n.unary_~
  //Modulus
  def unary_|(n: Complex) : Complex =  n.unary_!

  def unary_!(n: Complex) : Complex =  if (n.isZero) Complex.one else Complex.zero

  val table = Map("+" -> unary_+ _, "-" -> unary_- _, "~" -> unary_~ _, "|" -> unary_| _, "!" -> unary_! _)
}

object BinaryOperator {


  def + (left: Complex, right: Complex): Complex = left + right

  def - (left: Complex, right: Complex): Complex = left - right

  def * (left: Complex, right: Complex): Complex = left * right

  def / (left: Complex, right: Complex): Complex = left / right

  def ^ (left: Complex, right: Complex): Complex = left ^ right

  def && (left: Complex, right: Complex): Complex = if (!left.isZero && !right.isZero) Complex.one else Complex.zero

  def || (left: Complex, right: Complex): Complex = if(!left.isZero || !right.isZero) Complex.one else Complex.zero

  //XOR
  def ^^ (left: Complex, right: Complex): Complex = if(&&(left, right).isZero && ! ||(left, right).isZero) Complex.one else Complex.zero

  def < (left: Complex, right: Complex): Complex = if (left.re < right.re) Complex.one else Complex.zero
  def <= (left: Complex, right: Complex): Complex = if (left.re <= right.re) Complex.one else Complex.zero
  def > (left: Complex, right: Complex): Complex = if (left.re > right.re) Complex.one else Complex.zero
  def >= (left: Complex, right: Complex): Complex = if (left.re >= right.re) Complex.one else Complex.zero
  def == (left: Complex, right: Complex): Complex = if (left.re == right.re) Complex.one else Complex.zero

  val table = Map(("+", 2) -> BinaryOperator.+ _, ("-", 2) -> BinaryOperator.- _, ("*", 1) -> BinaryOperator.* _, ("/", 1) -> BinaryOperator./ _, ("^", 0) -> BinaryOperator.^ _,
    ("&&", 1) -> BinaryOperator.&& _, ("||", 2) -> BinaryOperator.|| _, ("^^", 2) -> BinaryOperator.^^ _, ("<", 2) -> BinaryOperator.< _, ("<=", 2) -> BinaryOperator.<= _,
    (">", 2) -> BinaryOperator.> _, (">=", 2) -> BinaryOperator.>= _, ("==", 2) -> BinaryOperator.== _)

}

object BuiltinFunction {

  def max (args: List[Complex]): Complex = {
    args match {
      case Nil => Complex.NaN
      case a :: Nil => a
      case a :: lst  => lst.foldLeft(a.re) {(s, c) => math.max(s, c.re)}
    }
  }

  def abs (args: List[Complex]): Complex = {
    args match {
      case List(a) => a.unary_!
      case _ => Complex.NaN
    }
  }

  def sin (args: List[Complex]): Complex = {
    args match {
      case List(a) if a.isReal => Complex(Math.sin(a.re))
      case List(a) => Complex(Math.sin(a.re) * Math.cosh(a.im), Math.cos(a.re) * Math.sinh(a.im))
      case _ => Complex.NaN
    }
  }

  val table = Map("max" -> max _, "abs" -> abs _, "sin" -> sin _)
}