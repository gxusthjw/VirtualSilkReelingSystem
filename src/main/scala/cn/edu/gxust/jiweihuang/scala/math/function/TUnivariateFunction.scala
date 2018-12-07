package cn.edu.gxust.jiweihuang.scala.math.function

import org.hipparchus.analysis.UnivariateFunction

trait TUnivariateFunction extends UnivariateFunction
  with Serializable {
  val formula: String
}
