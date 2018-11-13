/**
  * Created by manuel on 7/11/17.
  */
import org.scalatest.FlatSpec
import Parser._
import Utils._

class FirstSpec extends FlatSpec {

  val parser1 = new ExprParser(Set("x"))
  val parser2 = new ExprParser(Set("z"))
  val funcParser = new DefinitionParser()
  val varx = Map("x" -> Complex(5.0))
  val varz = Map("z" -> Complex(1, 3))

  "Simple expression 3 + 4" should "be 7.0" in {
    assert(parser1.evaluate("3+4")(varx) == Complex((7.0)))
  }

  "Simple expression 3-4" should "be -1.0" in {
    assert(parser1.evaluate("3-4")(varx) == Complex((-1.0)))
  }

  "Simple expression 3- 4" should "be -1.0" in {
    assert(parser1.evaluate("3- 4")(varx) == Complex((-1.0)))
  }

  "Simple expression 3*4" should "be 7.0" in {
    assert(parser1.evaluate("3*4")(varx) == Complex((12.0)))
  }

  "Simple expression 3i + 4" should "be 4 + 3i" in {
    assert(parser1.evaluate("3i+4")(varx) == Complex(4, 3))
  }

  "Simple expression 3i*4 + 3*4" should "be 12 + 12i" in {
    assert(parser1.evaluate("3i*4+3*4")(varx) == Complex(12, 12))
  }

  "Simple expression i + 0.5i*4 + 3*0.5" should "be 1.5 + 3i" in {
    assert(parser1.evaluate("i + 0.5i*4+3*0.5")(varx) == Complex(1.5, 3))
  }

  "Variable expression i + 0.5i*4 + x" should "be 5 + 3i" in {
    assert(parser1.evaluate("i+ 0.5i*4+x")(varx) == Complex(5, 3))
  }

  "Variable expression i + 0.5i*4 + 3 x" should "be 15 + 3i" in {
    assert(parser1.evaluate("i+ 0.5i*4+3 x")(varx) == Complex(15, 3))
  }

  "Variable expression i + 0.5i*4 + 3x" should "be 15 + 3i" in {
    assert(parser1.evaluate("i+ 0.5i*4+3x")(varx) == Complex(15, 3))
  }

  "Complex variable expression z + i + 0.5i*4 + 2z" should "be 5 + 3i" in {
    assert(parser2.evaluate("z+i+ 0.5i*4+2z")(varz) == Complex(3, 12))
  }

  "Exponential 4^3 " should "be 64" in {
    assert(parser1.evaluate("4^3")(varx) == Complex((math.pow(4.0, 3.0))))
  }

  "Exponential 36^0.5 " should "be 6" in {
    assert(parser1.evaluate("36^0.5")(varx) == Complex((6.0)))
  }

  "Exponential (2^2) (3^3) " should "be 108" in {
    assert(parser1.evaluate("(2^2) (3^3)")(varx) == Complex((108.0)))
  }
  //2^2x^3

  "Exponential 2^2*3^3 " should "be 108" in {
    assert(parser1.evaluate("2^2*3^3")(varx) == Complex((108.0)))
  }


  "Exponential e^pi " should "be printable" in {
    info(parser1.evaluate("e^pi")(varx).toString)
  }

  "Exponential e^i " should "be printable" in {
    info(parser1.evaluate("e^i")(varx).toString())
  }

  "Exponential i^3i " should "be printable" in {
    info(parser1.evaluate("i^3i")(varx).toString())
  }

  "Simple brackets (3 + 4)" should "be 7.0" in {
    assert(parser1.evaluate("(3+4)")(varx) == Complex((7.0)))
  }

  "Simple brackets 3*(4+5)" should "be 27.0" in {
    assert(parser1.evaluate("3*(4+5)")(varx) == Complex((27.0)))
  }

  "Simple expression i^(3i)" should "be printable" in {
    info(parser1.evaluate("i^(3i)")(varx).toString)
  }

  "Nested brackets 3 * (4 * (3 - 2))" should (" be " + (3*(4*(3-2))).toString) in {
    assert(parser1.evaluate("3*(4*(3-2))")(varx) == Complex((3*(4*(3-2)))))
  }

  "Nested brackets 3 * (4 * (3 - 2) + (2 - 3))" should (" be " + (3*(4*(3-2)+(2-3))).toString) in {
    assert(parser1.evaluate("3*(4*(3-2)+(2-3))")(varx) == Complex((3*(4*(3-2)+(2-3)))))
  }

  "Nested brackets with variables and constants 3 * (i * (3i - 2i) + (2 - 3))" should (" be " + (3*(-1+(2-3))).toString) in {
    assert(parser1.evaluate("3*(i*(3i-2i)+(2-3))")(varx) == Complex(3*(-1+(2-3)), 0))
  }

  "Simple function definition and call f [x] = x + 3 -> f[3+3]-1" should " be 8" in {
    val userF1 = Map("f" -> funcParser.define("def f[x] = x + 3").head.left.get)
    val parserF1 = new ExprParser(Set("x"), binaryOps = BinaryOperator.table, userFcts = userF1)
    assert(parserF1.evaluate("f[3+3]-1")(varx) == Complex((8.0)))
  }

  "Simple function definition and call f [x] = x + 3 if x >= 0; x - 3 if x < 0 -> f[1], f[-1]" should " be 4, -4" in {
    val userF1 = Map("f" -> funcParser.define("def f[x] = x + 3 if x >= 0; x - 3 if x < 0").head.left.get)
    val parserF1 = new ExprParser(Set("x"), binaryOps = BinaryOperator.table, userFcts = userF1)
    assert(parserF1.evaluate("f[1]")(varx) == Complex((4.0)))
    assert(parserF1.evaluate("f[-1]")(varx) == Complex((-4.0)))
  }

  "Builtin function 3 + max[2, 4]" should (" be " + (7).toString) in {
    assert(parser1.evaluate("3 + max[2, 4]")(varx) == Complex((7.0)))
  }

  "Builtin function 3 + max[4+1, -7]" should (" be " + (8).toString) in {
    assert(parser1.evaluate("3 + max[4+1, -7]")(varx) == Complex((8.0)))
  }

  "Random numbers between 1 and 10" should " be printable" in {
    info(parser1.evaluate("1 + (rnd * 9)")(varx).toString)
    info(parser1.evaluate("1 + (rnd * 9)")(varx).toString)
    info(parser1.evaluate("1 + (rnd * 9)")(varx).toString)
    info(parser1.evaluate("1 + (rnd * 9)")(varx).toString)
  }

  "Random complex numbers" should " be printable" in {
    info(parser1.evaluate("rndn + (rndn * i)")(varx).toString)
    info(parser1.evaluate("rndn + (rndn * i)")(varx).toString)
    info(parser1.evaluate("rndn + (rndn * i)")(varx).toString)
    info(parser1.evaluate("rndn + (rndn * i)")(varx).toString)
  }
}
