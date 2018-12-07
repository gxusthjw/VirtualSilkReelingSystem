package cn.edu.gxust.jiweihuang.scala.math.function.univariate

import cn.edu.gxust.jiweihuang.scala.math.NumericalConstants
import cn.edu.gxust.jiweihuang.scala.test.UnitSpec

class LogisticTest extends UnitSpec {
  /**
    *
    */
  "a Logistic constructor" should "throw new IllegalArgumentException if {logisticM = 0}" in {
    assertThrows[IllegalArgumentException] {
      TLogistic(0, 1, 2)
      TLogistic(0, 2, 3)
      TLogistic(0, 0, 3)
      TLogistic(0, 0, 0)
      TLogistic()
    }

  }

  "Logistic(1,2,3).formula" should "1.0 / (1 + exp(-2.0 * (x - 3.0)))" in {
    println(s"Logistic(1,2,3).formula = ${TLogistic(1, 2, 3).formula}")
    assertResult("1.0 / (1 + exp(-2.0 * (x - 3.0)))")(TLogistic(1, 2, 3).formula)
  }

  "Logistic.derivative" should "all equals to right value" in {
    println(s"Logistic(1,2,3).derivative(2.0) = ${TLogistic(1, 2, 3).derivative(2.0)}")
    println(s"Logistic.logisticDerivative(1, 2, 3)(2) = ${TLogistic.logisticDerivative(1, 2, 3)(2)}")
    println(s"Logistic(1, 2, 3).differential(2.0) = ${TLogistic(1, 2, 3).differential(2.0)}")
    assert(TLogistic.logisticDerivative(1, 2, 3)(2) - TLogistic(1, 2, 3).derivative(2) < NumericalConstants.NumericalPrecision)
    assert(TLogistic(1, 2, 3).differential(2.0) - TLogistic(1, 2, 3).derivative(2.0) < NumericalConstants.NumericalPrecision)
    val logistic = TLogistic(1, 2, 3)
    assert(logistic.derivative(0) - logistic.differential(0) <= NumericalConstants.NumericalPrecision)
    assert(logistic.derivative(100) - logistic.differential(100) <= NumericalConstants.NumericalPrecision)
    assert(logistic.derivative(-100) - logistic.differential(-100) <= NumericalConstants.NumericalPrecision)
    assert(logistic.derivative(200) - logistic.differential(200) <= NumericalConstants.NumericalPrecision)
    assert(logistic.derivative(-200) - logistic.differential(-200) <= NumericalConstants.NumericalPrecision)
    assert(logistic.derivative(10) - logistic.differential(10) <= NumericalConstants.NumericalPrecision)
    assert(logistic.derivative(5) - logistic.differential(5) <= NumericalConstants.NumericalPrecision)
  }

  "Logistic.integrate" should "all equals to right value" in {
    println(s"Logistic(1,2,3).integrate(2)=${TLogistic(1, 2, 3).integrate(2)}")

    println(s"Logistic.logisticIntegrate(1,2,3)(2.0)=${TLogistic.logisticIntegrate(1, 2, 3)(2.0)}")

    assert(TLogistic(1, 2, 3).integrate(2) - TLogistic.logisticIntegrate(1, 2, 3)(2.0) < NumericalConstants.NumericalPrecision)

    println(s"Logistic(1, 2, 3).integrateRomberg(0, 1200) = ${TLogistic(1, 2, 3).integrateRomberg(0, 1200)}")
    println(s"Logistic(1, 2, 3).integrateSimpson(0, 1200)=${TLogistic(1, 2, 3).integrateSimpson(0, 1200)}")
    println(s"Logistic(1, 2, 3).integrateMidPoint(0, 1200) = ${TLogistic(1, 2, 3).integrateMidPoint(0, 1200)}")
    println(s"Logistic(1, 2, 3).integrateTrapezoid(0, 1200) = ${TLogistic(1, 2, 3).integrateTrapezoid(0, 1200)}")
    println(s"Logistic(1, 2, 3).integrateIterativeLegendreGauss(0, 1200) = ${TLogistic(1, 2, 3).integrateIterativeLegendreGauss(0, 1200)}")

  }

  "Logistic.value" should "all equals to right value" in {
    println(s"Logistic(1,2,3).value(2)=${TLogistic(1, 2, 3).value(2)}")
    println(s"Logistic.logistic(1,2,3)(2)=${TLogistic.logistic(1, 2, 3)(2)}")
    assert(TLogistic(1, 2, 3).value(2) - TLogistic.logistic(1, 2, 3)(2) < NumericalConstants.NumericalPrecision)
  }



}
