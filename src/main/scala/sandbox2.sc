abstract class Acceptable[T]

object Acceptable {
  implicit object IntOk extends Acceptable[Int]
  implicit object LongOk extends Acceptable[Long]
}

def f[T: Acceptable](t: T) = t

f(1)