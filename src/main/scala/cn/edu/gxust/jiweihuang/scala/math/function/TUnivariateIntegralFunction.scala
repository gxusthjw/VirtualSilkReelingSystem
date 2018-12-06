package cn.edu.gxust.jiweihuang.scala.math.function

trait TUnivariateIntegralFunction extends TUnivariateFunction {

  def integrate(x: Double): Double

  def definiteIntegrate(lowerX: Double = lowerX, upperX: Double = upperX): Double = integrate(upperX) - integrate(lowerX)
}

object TUnivariateIntegralFunction {
  val integralConstant = 0.0
}