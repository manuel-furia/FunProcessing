package Chart
import MathFunction._
/**
  * Created by manuel on 7/10/17.
  *
  *
  */

case class Chart[-T,+O](val function : Func[T, O], val parameters: Parameters)

case class Color(r: Float, g: Float, b: Float)

object ChartType extends Enumeration {
  type ChartType = Value
  val Default, ComplexColors, ComplexVectors, VectorColors, VectorVectors = Value
}

import ChartType._

case class Parameters(val title: String = "",
                      val legend: String = "",
                      val pixelsSize: (Int, Int) = (512, 512),
                      val unitsSize: (Double, Double) = (10.0, 10.0),
                      val markingsResolution: Double = 1.0,
                      val foreColor: Color = Color(0,0,0),
                      val backColor: Color = Color(0.9F, 0.85F, 0.85F),
                      val moreProperties: Map[String, String] = Map(),
                      val referenceMax: Double = 10.0,
                      val chartType: ChartType = Default)

object Chart{
  val empty = Chart(Func.Empty, Parameters())
}