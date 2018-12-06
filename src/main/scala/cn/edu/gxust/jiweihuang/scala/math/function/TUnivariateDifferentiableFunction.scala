package cn.edu.gxust.jiweihuang.scala.math.function

import org.hipparchus.analysis.differentiation.{DSFactory, UnivariateDifferentiableFunction}

trait TUnivariateDifferentiableFunction extends TUnivariateFunction
  with UnivariateDifferentiableFunction {
  def differential(x: Double, order: Int = 1): Double =
    value(new DSFactory(1, order).variable(0, x)).getPartialDerivative(1)
}
