
package object Utils {

  object HSL {

    def hslToRgb(hue: Double, saturation: Double, lightness: Double) = {
      val h = hue

      def hue2rgb(p: Double, q: Double, t: Double): Double = {
        val tp = if (t < 0) t + 1.0 else if (t > 1.0) t - 1.0 else t
        if (tp < 1.0 / 6.0) p + (q - p) * 6.0 * tp
        else if (tp < 1.0 / 2.0) q
        else if (tp < 2.0 / 3.0) p + (q - p) * (2.0 / 3.0 - tp) * 6.0
        else p
      }

      if (saturation == 0) {
        (Math.round(lightness * 255).toInt,
          Math.round(lightness * 255).toInt,
          Math.round(lightness * 255).toInt)
      } else {
        val q = if (lightness < 0.5) lightness * (1.0 + saturation) else lightness + saturation - lightness * saturation
        val p = 2f * lightness - q
        val r = hue2rgb(p, q, h + 1.0 / 3.0)
        val g = hue2rgb(p, q, h)
        val b = hue2rgb(p, q, h - 1.0 / 3.0)
        (Math.round(r * 255).toInt, Math.round(g * 255).toInt, Math.round(b * 255).toInt)
      }
    }

  }

  case class Complex(val re: Double, val im: Double) {

    lazy val modulusSquare = math.pow(re, 2) + math.pow(im, 2)
    lazy val modulus = math.sqrt(modulusSquare)
    lazy val polar = (re * re + im * im, math.atan2(im, re))

    def this(re: Double) = this(re, 0)


    override def toString = this match {
      case Complex.i => "i"
      case Complex(re, 0.0) => re.toString
      case Complex(0.0, im) => im.toString + "i"
      case _ => re + (if (im < 0) "-" + -im else "+" + im) + "i"
    }

    def isZero: Boolean = re == 0.0 && im == 0.0
    def isReal: Boolean = im == 0.0
    def isNotDefined: Boolean = isNaN || isInfinity
    def isInfinity = re.isInfinity || im.isInfinity
    def isNaN = re.isNaN || im.isNaN

    def unary_+ = this

    def unary_- = new Complex(-re, -im)

    def unary_~ = new Complex(re, -im)

    def unary_! = modulus

    def +(c: Complex) = new Complex(re + c.re, im + c.im)

    def -(c: Complex) = new Complex(re - c.re, im - c.im)

    def *(c: Complex) = new Complex(re * c.re - im * c.im, im * c.re + re * c.im)

    def *(d: Double) = new Complex(re * d, im * d)

    def ==(c: Complex): Boolean = re == c.re && im == c.im

    def /(c: Complex) = {
      require(c.re != 0 || c.im != 0)
      val d = math.pow(c.re, 2) + math.pow(c.im, 2)
      new Complex((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
    }

    def /(d: Double) = new Complex(re / d, im / d)

    def logc = {
      Complex(math.log(this.modulus), this.polar._2)
    }

    def ^(c: Complex) = {
      //Avoid passing by polar coordinates if not needed
      if (this.isReal && c.isReal)
        Complex(math.pow(this.re, c.re))
      else {
        val (rThis, tThis) = this.polar
        val (rThat, tThat) = c.polar
        val wlogcz = c * this.logc
        //e^wlogcz = e^(x + iy) = e^x e^iy = (cos(y) + isin(y)) * e^x
        Complex.fromPolar(1.0, wlogcz.im) * math.pow(math.E, wlogcz.re)
      }
    }

    def ^(d: Double) = {
      //Avoid passing by polar coordinates if not needed
      if (this.isReal)
        Complex(math.pow(this.re, d))
      else {
        val (r, t) = this.polar
        Complex.fromPolar(math.pow(r, d), t * d)
      }
    }


  }

  object Complex {
    val i = new Complex(0, 1)
    val one = new Complex(1)
    val zero = new Complex(0, 0)
    val NaN = new Complex(Double.NaN, Double.NaN)

    def apply(re: Double) = new Complex(re, 0)

    //def unapply(c: Complex): Option[(Double, Double)] = Some(c.re, c.im)

    implicit def fromDouble(d: Double) = new Complex(d, 0)

    implicit def fromFloat(f: Float) = new Complex(f, 0)

    implicit def fromLong(l: Long) = new Complex(l, 0)

    implicit def fromInt(i: Int) = new Complex(i, 0)

    implicit def fromShort(s: Short) = new Complex(s, 0)

    def fromPolar(r: Double, t: Double) = new Complex(r * math.cos(t), r * math.sin(t))
  }

}