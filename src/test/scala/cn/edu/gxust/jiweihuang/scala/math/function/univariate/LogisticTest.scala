package cn.edu.gxust.jiweihuang.scala.math.function.univariate

import cn.edu.gxust.jiweihuang.scala.math.NumericalConstants
import cn.edu.gxust.jiweihuang.scala.test.UnitSpec

class LogisticTest extends UnitSpec {
  "a Logistic constructor" should "throw new IllegalArgumentException if {logisticM = 0}" in {
    assertThrows[IllegalArgumentException] {
      Logistic(0, 1, 2)
      Logistic(0, 2, 3)
    }
  }

  "Logistic(1,2,3).formula" should "1.0 / (1 + exp(-2.0 * (x - 3.0)))" in {
    println(s"Logistic(1,2,3).formula = ${Logistic(1, 2, 3).formula}")
    assertResult("1.0 / (1 + exp(-2.0 * (x - 3.0)))")(Logistic(1, 2, 3).formula)
  }

  "Logistic.derivative" should "all equals to right value" in {
    println(s"Logistic(1,2,3).derivative(2.0) = ${Logistic(1, 2, 3).derivative(2.0)}")
    println(s"Logistic.logisticDerivative(1, 2, 3)(2) = ${Logistic.logisticDerivative(1, 2, 3)(2)}")
    println(s"Logistic(1, 2, 3).differential(2.0) = ${Logistic(1, 2, 3).differential(2.0)}")
    assert(Logistic.logisticDerivative(1, 2, 3)(2) - Logistic(1, 2, 3).derivative(2) < NumericalConstants.NumericalPrecision)
    assert(Logistic(1, 2, 3).differential(2.0) - Logistic(1, 2, 3).derivative(2.0) < NumericalConstants.NumericalPrecision)
  }

  "Logistic.integrate" should "all equals to right value" in {
    println(s"Logistic(1,2,3).integrate(2)=${Logistic(1, 2, 3).integrate(2)}")

    println(s"Logistic.logisticIntegrate(1,2,3)(2.0)=${Logistic.logisticIntegrate(1, 2, 3)(2.0)}")

    assert(Logistic(1, 2, 3).integrate(2) - Logistic.logisticIntegrate(1, 2, 3)(2.0) < NumericalConstants.NumericalPrecision)

    println(s"Logistic(1, 2, 3).integrateRomberg(0, 1200) = ${Logistic(1, 2, 3).integrateRomberg(0, 1200)}")
    println(s"Logistic(1, 2, 3).integrateSimpson(0, 1200)=${Logistic(1, 2, 3).integrateSimpson(0, 1200)}")
    println(s"Logistic(1, 2, 3).integrateMidPoint(0, 1200) = ${Logistic(1, 2, 3).integrateMidPoint(0, 1200)}")
    println(s"Logistic(1, 2, 3).integrateTrapezoid(0, 1200) = ${Logistic(1, 2, 3).integrateTrapezoid(0, 1200)}")
    println(s"Logistic(1, 2, 3).integrateIterativeLegendreGauss(0, 1200) = ${Logistic(1, 2, 3).integrateIterativeLegendreGauss(0, 1200)}")

  }

  "Logistic.value" should "all equals to right value" in {
    println(s"Logistic(1,2,3).value(2)=${Logistic(1, 2, 3).value(2)}")
    println(s"Logistic.logistic(1,2,3)(2)=${Logistic.logistic(1, 2, 3)(2)}")
    assert(Logistic(1, 2, 3).value(2) - Logistic.logistic(1, 2, 3)(2) < NumericalConstants.NumericalPrecision)
  }

}
