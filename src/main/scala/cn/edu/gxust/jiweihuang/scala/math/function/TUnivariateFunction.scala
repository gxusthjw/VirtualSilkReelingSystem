package cn.edu.gxust.jiweihuang.scala.math.function

import org.hipparchus.analysis.UnivariateFunction

trait TUnivariateFunction extends UnivariateFunction {
  val lowerX: Double = Double.MinValue
  val upperX: Double = Double.MaxValue
  val formula: String

  def checkX(x: Double): Boolean = (x < lowerX && x > upperX)
}
