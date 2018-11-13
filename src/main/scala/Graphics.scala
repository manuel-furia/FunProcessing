package Graphics
import Chart._
import MathFunction._
import Utils._
import processing.core.{PApplet, PConstants}
import processing.core.PConstants.P3D


/**
  * Created by manuel on 7/10/17.
  */
class Graphics(charts: List[Chart[_, _]]) extends PApplet{

  def dline (x1: Double, y1: Double, x2: Double, y2: Double): Unit = this.line(x1.toFloat, y1.toFloat, x2.toFloat, y2.toFloat)

  def coordToPixels(ux: Double, uy: Double): (Double, Double) = {
    (w/2 + ux * pux, h/2 + uy * puy)
  }

  private val baseChart = charts.headOption.getOrElse(Chart.empty)
  private val Color(unitR, unitG, unitB) = baseChart.parameters.backColor
  private val Color(unitFR, unitFG, unitFB) = baseChart.parameters.foreColor
  private val (br, bb, bg) = (unitR * 255, unitB * 255, unitG * 255)
  private val (fr, fb, fg) = (unitFR * 255, unitFB * 255, unitFG * 255)
  private val (w, h) = baseChart.parameters.pixelsSize
  private val (uw, uh) = baseChart.parameters.unitsSize
  private val (pux, puy) = (w/uw, h/uh) //Pixels per unit (X and Y)
  private val markRes = baseChart.parameters.markingsResolution

  //val func = baseChart.function

  val values: Map[Chart[_, _], Any] = charts.map { c =>
    c -> (c.function match {
      case f: RtoRFunc => {

        val margin = 0.55
        val resolutionPerPx = 5.0 * margin * 2
        val range = (-uw * margin, uw * margin)
        val rangeSize = range._2 - range._1
        val increment = rangeSize / (resolutionPerPx * w)
        val number = (rangeSize / increment).toInt
        val variable = f.parsed._2.headOption.getOrElse("x")

        def func(x: Double) = f.apply(variable, x)
        Array.iterate((range._1, func(range._1)), number)(x => (x._1 + increment, func(x._1)))

      }
      case f: CtoCFunc => {

        val margin = 0.50
        val resolutionPerPx = 1.0 * margin * 2
        val (rangeX, rangeY) = ((-uw * margin, uw * margin), (-uh * margin, uh * margin))
        val (rangeXSize, rangeYSize) = (rangeX._2 - rangeX._1, rangeY._2 - rangeY._1)
        val (incrementX, incrementY) = (rangeXSize / (resolutionPerPx * w), rangeYSize / (resolutionPerPx * h))
        val (numberX, numberY) = ((rangeXSize / incrementX).toInt, (rangeYSize / incrementY).toInt)
        val variable = f.parsed._2.headOption.getOrElse("z")

        def nmComplex(n: Int, m: Int) = Complex(rangeX._1 + n * incrementX, rangeY._1 + m * incrementY)

        Array.tabulate(numberX * numberY){(n) =>
          val x = nmComplex(n % numberX, n / numberX)
          val y = f.apply(variable, x)
          (x, y)
        }

      }
    })
  }.toMap


  def run() : Unit = {
    this.runSketch()
  }

  override def setup() : Unit = {
    background(br, bg, bb)
    stroke(fr, fg, fb)
    strokeWeight(1F)
    //frameRate(30)
    noLoop()
    AAShader.initFXAA(this)
  }

  override def settings() : Unit = {

    size(w, h, PConstants.P2D)
    smooth(8)

  }

  override def draw() : Unit = {
    background(br, bg, bb)
    drawGrid2D()
    charts.foreach(dispatchPlot(_))

    if (mousePressed) {
      line(mouseX,mouseY,pmouseX,pmouseY)
    }

    if (AAShader.fxaa.nonEmpty)
      filter(AAShader.fxaa.get)
  }

  def dispatchPlot(chart: Chart[_, _]): Unit = {

    val Color(unitR, unitG, unitB) = baseChart.parameters.backColor
    val Color(unitFR, unitFG, unitFB) = chart.parameters.foreColor
    val (br, bb, bg) = (unitR * 255, unitB * 255, unitG * 255)
    val (fr, fb, fg) = (unitFR * 255, unitFB * 255, unitFG * 255)


    chart.function match {
      case f: RtoRFunc => plot2DRtoR()
      case f: CtoCFunc => {
        chart.parameters.chartType match {
          case ChartType.Default => plotColorsCtoC()
          case ChartType.ComplexColors => plotColorsCtoC()
          case _ => throw new Exception("Error: Unknown chart type.")
        }
      }
      case _ => throw new Exception("Error: Unknown kind of function.")
    }

    def plot2DRtoR(): Unit = {

      stroke(fr,fg,fb)

      values(chart) match {
        case vals: Array[(Double, Double)] => vals.reduceLeft {(v, newv) =>
          val (pxA, pyA) = coordToPixels(v._1, v._2)
          val (pxB, pyB) = coordToPixels(newv._1, newv._2)

          dline(pxA, pyA, pxB, pyB)
          newv
        }
        case _ => throw new Exception("Internal Error: dispatchPlot().plot2DRtoR()")
      }

    }

    def plotColorsCtoC(): Unit = {

      def colorFromComplex(z: Complex, max: Double) = {
        def sigmoid(x: Double) = 1 / (1 + Math.pow(Math.E, -x))
        val (r, angle) = z.polar
        HSL.hslToRgb(angle / Math.PI, 1.0, sigmoid(r/(max/3)) - 0.5)
      }

      values(chart) match {

        case vals: Array[(Complex, Complex)] =>

          loadPixels()
          vals.foreach {v =>
            val (px, py) = coordToPixels(v._1.re, v._1.im)
            val (r, g, b) = colorFromComplex(v._2, chart.parameters.referenceMax)
            val index = px.toInt + py.toInt*w

            stroke(r, g, b)
            if (index < h*w && index >= 0)
              pixels(index) = color(r, g, b)
            }
          updatePixels()
          stroke(fr,fg,fb)

        case _ => throw new Exception("Internal Error: dispatchPlot().plotColorsCtoC()")
      }

    }

  }



  def drawGrid2D(): Unit = {

    stroke(fr,fg,fb)

    def chooseAndStroke(light : Boolean)(code : => Unit) = {
      if (light) stroke((fr+br*4)/5, (fg+bg*4)/5, (fb+bb*4)/5)
      code
      stroke(fr, fg, fb)
    }

    val (sx, sy) = (pux / markRes, puy / markRes) //Square size x and y
    val (lux, luy) = ((w / sx).toInt, (h / sy).toInt) //Number of lines in the X and Y directions

    for (i <- -(lux/2) until lux/2) {
      chooseAndStroke (i!=0) {
        dline(i*sx + w/2, 0, i*sx + w/2, h)
      }
    }
    for (j <- -(luy/2) until luy/2) {
      chooseAndStroke(j!=0){
        dline(0, j * sy + h / 2, w, j * sy + h / 2)
      }
    }
  }

}
