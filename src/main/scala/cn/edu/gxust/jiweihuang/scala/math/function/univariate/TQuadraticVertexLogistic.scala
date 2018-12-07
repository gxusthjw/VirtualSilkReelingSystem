package cn.edu.gxust.jiweihuang.scala.math.function.univariate

import cn.edu.gxust.jiweihuang.scala.math.function.{TUnivariateDerivativeFunction, TUnivariateDifferentiableFunction, TUnivariateIntegrableFunction, TUnivariateIntegralFunction}
import org.hipparchus.analysis.ParametricUnivariateFunction
import org.hipparchus.analysis.differentiation.DerivativeStructure

import scala.math._

trait TQuadraticVertexLogistic extends TUnivariateDifferentiableFunction
  with TUnivariateIntegrableFunction
  with TUnivariateDerivativeFunction
  with TUnivariateIntegralFunction {
  override val formula: String = s"($quadraticVertexA*pow(x-$quadraticVertexB,2)+$quadraticVertexC)*$logisticM / (1 + exp(-$logisticK*(x-$logisticX0))) + $quadraticVertexLogisticD"
  val quadraticVertexA: Double
  val quadraticVertexB: Double
  val quadraticVertexC: Double
  val quadraticVertex: TQuadraticVertex = QuadraticVertex(quadraticVertexA, quadraticVertexB, quadraticVertexC)
  val logisticM: Double
  val logisticK: Double
  val logisticX0: Double
  val logistic: TLogistic = Logistic(logisticM, logisticK, logisticX0)
  val quadraticVertexLogisticD: Double

  override def derivative(x: Double): Double = quadraticVertex.derivative(x) * logistic.value(x) + logistic.derivative(x) * quadraticVertex.value(x)

  /**
    * omitted the Polylog part.
    *
    * @param x independent variable.
    * @return the integral value.
    */
  override def integrate(x: Double): Double = quadraticVertexLogisticD * x + quadraticVertexA * logisticM * pow(x, 3) / 3 -
    2 * quadraticVertexA * quadraticVertexB * logisticM * x * log(1 + exp(logisticK * (x - logisticX0))) / logisticK +
    quadraticVertexA * pow(quadraticVertexB, 2) * logisticM * log(exp(logisticK * x) + exp(logisticK * logisticX0)) / logisticK +
    quadraticVertexC * logisticM * log(exp(logisticK * x) + exp(logisticK * logisticX0)) / logisticK +
    quadraticVertexA * logisticM * pow(x, 2) * log(1 + exp(logisticK * (logisticX0 - x))) / logisticK

  override def value(ds: DerivativeStructure): DerivativeStructure = quadraticVertex.value(ds).multiply(logistic.value(ds)).add(quadraticVertexLogisticD)

  override def value(x: Double): Double = logistic.value(x) * quadraticVertex.value(x) + quadraticVertexLogisticD

}

object TQuadraticVertexLogistic {

  final class Parametric extends ParametricUnivariateFunction {
    override def value(x: Double, parameters: Double*): Double = {
      checkParameter(parameters: _*)
      val a = parameters(0)
      val b = parameters(1)
      val c = parameters(2)
      val m = parameters(3)
      val k = parameters(4)
      val x0 = parameters(5)
      val d = parameters(6)
      TQuadraticVertexLogistic.quadraticVertexLogistic(a, b, c, m, k, x0, d)(x)
    }

    override def gradient(x: Double, parameters: Double*): Array[Double] = {
      checkParameter(parameters: _*)
      val a = parameters(0)
      val b = parameters(1)
      val c = parameters(2)
      val m = parameters(3)
      val k = parameters(4)
      val x0 = parameters(5)
      val d = parameters(6)
      val result = Array[Double](7)
      import TQuadraticVertexLogistic._
      result(0) = quadraticVertexLogisticDerivativeA(a, b, c, m, k, x0, d)(x)
      result(1) = quadraticVertexLogisticDerivativeB(a, b, c, m, k, x0, d)(x)
      result(2) = quadraticVertexLogisticDerivativeC(a, b, c, m, k, x0, d)(x)
      result(3) = quadraticVertexLogisticDerivativeM(a, b, c, m, k, x0, d)(x)
      result(4) = quadraticVertexLogisticDerivativeK(a, b, c, m, k, x0, d)(x)
      result(5) = quadraticVertexLogisticDerivativeX0(a, b, c, m, k, x0, d)(x)
      result(6) = quadraticVertexLogisticDerivativeD(a, b, c, m, k, x0, d)(x)
      result
    }

    def checkParameter(parameters: Double*): Unit = {
      if (parameters == null) throw new IllegalArgumentException(s"Expected the parameter {parameters != null},but got {parameters = null}}")
      if (parameters.length != 7) throw new IllegalArgumentException(s"Expected the parameter {parameters.length == 7},but got {parameters.length = ${parameters.length}}")
      if (parameters.head == 0) throw new IllegalArgumentException(s"Expected the parameter {parameters(0) != 0},but got {parameters(0) = ${parameters.head}}")
      if (parameters(3) == 0) throw new IllegalArgumentException(s"Expected the parameter {parameters(3) != 0},but got {parameters(0) = ${parameters.head}}")
    }
  }

  def apply(quadraticVertexA: Double = 1.0,
            quadraticVertexB: Double = 0.0,
            quadraticVertexC: Double = 0.0,
            logisticM: Double = 1.0,
            logisticK: Double = -1.0,
            logisticX0: Double = 0.0,
            quadraticVertexLogisticD: Double = 0.0): TQuadraticVertexLogistic =
    QuadraticVertexLogistic(quadraticVertexA, quadraticVertexB, quadraticVertexC, logisticM,
      logisticK, logisticX0, quadraticVertexLogisticD)

  def unapply(quadraticVertexLogistic: TQuadraticVertexLogistic): Option[(Double, Double, Double, Double, Double, Double, Double)] =
    if (quadraticVertexLogistic == null) None
    else Some(quadraticVertexLogistic.quadraticVertexA,
      quadraticVertexLogistic.quadraticVertexB,
      quadraticVertexLogistic.quadraticVertexC,
      quadraticVertexLogistic.logisticM,
      quadraticVertexLogistic.logisticK,
      quadraticVertexLogistic.logisticX0,
      quadraticVertexLogistic.quadraticVertexLogisticD)

  def quadraticVertexLogistic(a: Double, b: Double, c: Double, m: Double, k: Double, x0: Double, d: Double)(x: Double): Double = {
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TLogistic._
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TQuadraticVertex._
    quadraticVertex(a, b, c)(x) * logistic(m, k, x0)(x) + d
  }

  def quadraticVertexLogisticDerivative(a: Double, b: Double, c: Double, m: Double, k: Double, x0: Double, d: Double)(x: Double): Double = {
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TLogistic._
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TQuadraticVertex._
    quadraticVertexDerivative(a, b, c)(x) * logistic(m, k, x0)(x) + logisticDerivative(m, k, x0)(x) * quadraticVertex(a, b, c)(x)
  }

  def quadraticVertexLogisticDerivativeA(a: Double, b: Double, c: Double, m: Double, k: Double, x0: Double, d: Double)(x: Double): Double = {
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TLogistic._
    pow(x - b, 2) * logistic(m, k, x0)(x)
  }

  def quadraticVertexLogisticDerivativeB(a: Double, b: Double, c: Double, m: Double, k: Double, x0: Double, d: Double)(x: Double): Double = {
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TLogistic._
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TQuadraticVertex._
    quadraticVertexDerivative(a, b, c)(x) * logistic(m, k, x0)(x)
  }

  def quadraticVertexLogisticDerivativeC(a: Double, b: Double, c: Double, m: Double, k: Double, x0: Double, d: Double)(x: Double): Double = {
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TLogistic._
    logistic(m, k, x0)(x)
  }

  def quadraticVertexLogisticDerivativeM(a: Double, b: Double, c: Double, m: Double, k: Double, x0: Double, d: Double)(x: Double): Double = {
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TLogistic._
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TQuadraticVertex._
    quadraticVertex(a, b, c)(x) / logisticExpAddOne(k, x0)(x)
  }

  def quadraticVertexLogisticDerivativeK(a: Double, b: Double, c: Double, m: Double, k: Double, x0: Double, d: Double)(x: Double): Double = {
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TLogistic._
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TQuadraticVertex._
    logisticExp(k, x0)(x) * quadraticVertex(a, b, c)(x) * (x - x0) * m / pow(logisticExpAddOne(k, x0)(x), 2)
  }

  def quadraticVertexLogisticDerivativeX0(a: Double, b: Double, c: Double, m: Double, k: Double, x0: Double, d: Double)(x: Double): Double = {
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TLogistic._
    import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TQuadraticVertex._
    -logisticExp(k, x0)(x) * quadraticVertex(a, b, c)(x) * k * m / pow(logisticExpAddOne(k, x0)(x), 2)
  }

  def quadraticVertexLogisticDerivativeD(a: Double, b: Double, c: Double, m: Double, k: Double, x0: Double, d: Double)(x: Double): Double = 1
}