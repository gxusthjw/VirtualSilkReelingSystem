package cn.edu.gxust.jiweihuang.scala.math.function

trait TUnivariateIntegralFunction extends TUnivariateFunction {
  //val integralConstant = 0.0
  def integrate(x: Double): Double

  def definiteIntegrate(lowerX: Double = lowerX, upperX: Double = upperX): Double = integrate(upperX) - integrate(lowerX)
}
