package cn.edu.gxust.jiweihuang.scala.math.function.univariate

import cn.edu.gxust.jiweihuang.scala.math.function.{TUnivariateDerivativeFunction, TUnivariateDifferentiableFunction, TUnivariateIntegrableFunction, TUnivariateIntegralFunction}
import org.hipparchus.analysis.ParametricUnivariateFunction
import org.hipparchus.analysis.differentiation.DerivativeStructure

import scala.math.{exp, log, pow}


/**
  * <p>The class [[Logistic]] is used for representing
  * the logistic function of which formula is
  * {{{l(x) = m/(1+exp(-k*(x-x0)))}}}.</p>
  *
  * @param logisticM  The parameter {m} of logistic function.
  * @param logisticK  The parameter {k} of logistic function.
  * @param logisticX0 The parameter {x0} of logistic function.
  */
final class Logistic(val logisticM: Double = 1.0,
                     val logisticK: Double = -1.0,
                     val logisticX0: Double = 0.0)
  extends TUnivariateDifferentiableFunction
    with TUnivariateIntegrableFunction
    with TUnivariateDerivativeFunction
    with TUnivariateIntegralFunction {

  if (logisticM == 0) throw new IllegalArgumentException(s"Expected the parameter {logisticM != 0},but get {logisticM = $logisticM}")

  /**
    * The string form of analysis formula of univariate function.
    */
  override val formula: String = s"$logisticM / (1 + exp(-$logisticK * (x - $logisticX0)))"

  /**
    * <p>The method {{{derivative(x: Double)}}} is used to
    * get the derivative value of analysis univariate derivative function.</p>
    *
    * @param x independent variable.
    * @return the derivative value.
    */
  override def derivative(x: Double): Double = (logisticM * logisticK * logisticExp(x)) / pow(logisticExpAddOne(x), 2)

  /**
    * The method {{{logisticExpAddOne(x: Double)}}} is used to
    * calculate one add exp part ({{{1+exp(-k*(x-x0))}}})of logistic function.
    *
    * @param x independent variable.
    * @return the calculate value of {1+exp(-k*(x-x0))}.
    */
  def logisticExpAddOne(x: Double): Double = 1 + logisticExp(x)

  /**
    * to calculate the exp part ({{{exp(-k*(x-x0))}}}) of logistic function.
    *
    * @param x independent variable.
    * @return the calculate value of {{{exp(-k*(x-x0))}}}
    **/
  def logisticExp(x: Double): Double = exp(-logisticK * (x - logisticX0))

  /**
    * The method {{{integrate(x: Double)}}} is used to get
    * the integral value of analysis univariate integral function.
    *
    * @param x independent variable.
    * @return the integral value.
    */
  override def integrate(x: Double): Double = logisticM * (x + (log(logisticExpAddOne(x)) / logisticK))

  /**
    * the method {{{value(t: DerivativeStructure)}}} is used to get
    * the [[[DerivativeStructure]]] form of function for {{{differential()}}}.
    *
    * @param t the [[DerivativeStructure]] form of independent variable.
    * @return the [[DerivativeStructure]] form of function.
    */
  override def value(t: DerivativeStructure): DerivativeStructure =
    t.subtract(logisticX0).multiply(-logisticK).exp().add(1).pow(-1).multiply(logisticM)

  /**
    * the method {{{value(x: Double)}}} is used to get
    * the function value at {{{x}}}.
    *
    * @param x independent variable.
    * @return the function value at {{{x}}}.
    */
  override def value(x: Double): Double =logisticM / (1 + exp(-logisticK * (x - logisticX0)))


  final class Parametric extends ParametricUnivariateFunction {
    override def value(x: Double, parameters: Double*): Double = {
      checkParameter(parameters: _*)
      val m = parameters(0)
      val k = parameters(1)
      val x0 = parameters(2)
      m / (1 + exp(-k * (x - x0)))
    }

    override def gradient(x: Double, parameters: Double*): Array[Double] = {
      checkParameter()
      val m = parameters(0)
      val k = parameters(1)
      val x0 = parameters(2)
      val result = Array[Double](3)
      result(0) = 1 / (1 + exp(-k * (x - x0)))
      result(1) = -(exp(-k * (x - x0)) * m * (x0 - x)) / pow(1 + exp(-k * (x - x0)), 2)
      result(2) = -(exp(-k * (x - x0)) * k * m) / pow(1 + exp(-k * (x - x0)), 2)
      result
    }

    def checkParameter(parameters: Double*): Unit = {
      if (parameters == null) throw new IllegalArgumentException(s"Expected the parameter {parameters != null},but got {parameters = null}}")
      if (parameters.length != 3) throw new IllegalArgumentException(s"Expected the parameter {parameters.length == 3},but got {parameters.length = ${parameters.length}}")
      if (parameters.head == 0) throw new IllegalArgumentException(s"Expected the parameter {parameters(0) != 0},but got {parameters(0) = ${parameters.head}}")
    }
  }

  override def equals(other: Any): Boolean = other match {
    case that: Logistic =>
      (that canEqual this) &&
        logisticM == that.logisticM &&
        logisticK == that.logisticK &&
        logisticX0 == that.logisticX0
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(logisticM, logisticK, logisticX0)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  /**
    * whether {{{other}}} is instance of class [[Logistic]]
    *
    * @param other another instance of class [[Logistic]]
    * @return {{{Boolean}}} for whether {{{other}}} is instance of class [[Logistic]]
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[Logistic]
}

object Logistic {
  def apply(logisticM: Double = 1.0, logisticK: Double = -1.0, logisticX0: Double = 0.0): Logistic = new Logistic(logisticM, logisticK, logisticX0)

  def unapply(logistic: Logistic): Option[(Double, Double, Double)] =
    if (logistic == null) None
    else Some(logistic.logisticM, logistic.logisticK, logistic.logisticX0)

  def logistic(logisticM: Double = 1.0, logisticK: Double = -1.0, logisticX0: Double = 0.0)(x: Double): Double = logisticM / logisticExpAddOne(logisticK, logisticX0)(x)

  def logisticDerivative(logisticM: Double = 1.0, logisticK: Double = -1.0, logisticX0: Double = 0.0)(x: Double): Double = (logisticM * logisticK * logisticExp(logisticK, logisticX0)(x)) / pow(logisticExpAddOne(logisticK, logisticX0)(x), 2)

  def logisticExp(logisticK: Double = -1.0, logisticX0: Double = 0.0)(x: Double): Double = exp(-logisticK * (x - logisticX0))

  def logisticExpAddOne(logisticK: Double = -1.0, logisticX0: Double = 0.0)(x: Double): Double = 1 + logisticExp(logisticK, logisticX0)(x)

  def logisticIntegrate(logisticM: Double = 1.0, logisticK: Double = -1.0, logisticX0: Double = 0.0)(x: Double): Double = logisticM * (x + log(logisticExpAddOne(logisticK, logisticX0)(x)) / logisticK)

  def logisticDerivativeM(logisticM: Double = 1.0, logisticK: Double = -1.0, logisticX0: Double = 0.0)(x: Double): Double = 1 / logisticExpAddOne(logisticK, logisticX0)(x)

  def logisticDerivativeK(logisticM: Double = 1.0, logisticK: Double = -1.0, logisticX0: Double = 0.0)(x: Double): Double = -(logisticM * (-x + logisticX0) * logisticExp(logisticK, logisticM)(x)) / pow(logisticExpAddOne(logisticK, logisticX0)(x), 2)

  def logisticDerivativeX0(logisticM: Double = 1.0, logisticK: Double = -1.0, logisticX0: Double = 0.0)(x: Double): Double = -(logisticK * logisticM * logisticExp(logisticK, logisticX0)(x)) / pow(logisticExpAddOne(logisticK, logisticX0)(x), 2)
}