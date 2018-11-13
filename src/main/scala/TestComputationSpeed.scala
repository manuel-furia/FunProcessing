package Profile
/**
  * Created by manuel on 7/31/17.
  */
import Utils.Complex
import MathFunction.{CtoCFunc, Func, RtoRFunc}

object TestComputationSpeed {

  private val rndGen = new scala.util.Random
  def rnd = rndGen.nextDouble()

  def timeExpr[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) + "ns")
    result
  }

  def timeUnit[R](block: => R): Double = {
    val t0 = System.nanoTime()
    block   // call-by-name
    val t1 = System.nanoTime()
    t1 - t0
  }


  def doubleTime(): Double = {
    val lst = (1 to 5000000).map(x => rnd).toList
    timeUnit {
      val input2 = lst.product
      val input3 = lst.reduce((x, y) => if (y != 0) x / y else x)
      lst.sum
    } / 1000000 //ms
  }

  def complexTime(): Double = {
    val lst = (1 to 5000000).map(x => new Complex(rnd, rnd)).toList
    timeUnit {
      val input2 = lst.reduce((x, y) => x * y)
      val input3 = lst.reduce((x, y) => if (!y.isZero) x / y else x)
      lst.reduce(_ + _)
    } / 1000000 //ms
  }

  def functionExecTime[T, O](f: Func[T, O], start: Double, increment: Double, n: Integer, parallel: Boolean = false): Double = {
    val nums = Array.tabulate(n)(i => start + increment)
    f match {
      case func : RtoRFunc => {
        timeUnit {
          if (parallel) nums.par.foreach(x => func.apply("x", x))
          else nums.foreach(x => func.apply("x", x))
        } / 1000000 //ms
      }
      case func : CtoCFunc => {
        timeUnit {
          if (parallel) nums.par.foreach(x => func.apply("x", x))
          else nums.foreach(x => func.apply("x", Complex(x)))
        } / 1000000 //ms
      }
      case _ => Double.NaN
    }
  }

}
