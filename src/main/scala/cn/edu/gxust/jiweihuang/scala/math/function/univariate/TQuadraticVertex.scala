package cn.edu.gxust.jiweihuang.scala.math.function.univariate

import org.hipparchus.analysis.ParametricUnivariateFunction
import org.hipparchus.analysis.differentiation.DerivativeStructure

import scala.math._

/**
  * <p>The class [[TQuadraticVertex]] is used for representing
  * the vertex form of quadratic function of which formula is
  * {{{q(x)=a*(x-b)^2+c}}}</p>
  *
  * @param quadraticVertexA the parameter {a} of the vertex form of quadratic function.
  * @param quadraticVertexB the parameter {b} of the vertex form of quadratic function.
  * @param quadraticVertexC the parameter {c} of the vertex form of quadratic function.
  * @see TQuadratic
  */
trait TQuadraticVertex extends TQuadratic {
  val quadraticVertexA: Double = 1.0
  val quadraticVertexB: Double = 0.0
  val quadraticVertexC: Double = 0.0
  /**
    * Ensure the parameters {{{quadraticVertexA != 0}}}
    */
  if (quadraticVertexA == 0) throw new IllegalArgumentException(s"Expected the parameter {quadraticVertexA != 0},but got {quadraticVertexA = $quadraticVertexA}.")
  /**
    * The vertex coordinates {{{(x,y)}}} of quadratic function
    */
  override val vertex: Array[Double] = Array(quadraticVertexB, quadraticVertexC)
  /**
    * <p>whether quadratic function is invert.</p>
    * <p>if invert (i.e. open side down) return true else return false.</p>
    */
  override val isInvert: Boolean = if (quadraticVertexA < 0) true else false
  /**
    * the x coordinate of intersection with x axis of function
    */
  override val xIntersection: Array[Double] = {
    val tem = -quadraticVertexC / quadraticVertexA
    if (tem > 0) Array[Double](quadraticVertexB - sqrt(tem), quadraticVertexB + sqrt(tem))
    else if (tem == 0) Array[Double](quadraticVertexB) else Array[Double]()
  }
  /**
    * the y coordinate of intersection with y axis of function
    */
  override val yIntersection: Double = quadraticVertexA * pow(quadraticVertexB, 2) +
    quadraticVertexC

  /**
    * The number of intersection with x axis of function
    */
  override val xIntersectionNum: Int = xIntersection.length
  /**
    * The String form of univariate function.
    */
  override val formula: String = s"$quadraticVertexA * pow(x - $quadraticVertexB, 2) + $quadraticVertexC"

  /**
    * <p>override the method of [[org.hipparchus.analysis.UnivariateFunction]],it can be used to
    * get the value of univariate function.</p>
    *
    * @param x independent variable
    * @return the value of univariate function.
    */
  override def value(x: Double): Double = quadraticVertexA * pow(x - quadraticVertexB, 2) + quadraticVertexC

  /**
    * <p>override the method of [[org.hipparchus.analysis.differentiation.UnivariateDifferentiableFunction]],
    * it can be used to get the differential value (i.e. derivative value) of
    * univariate function.</p>
    *
    * @param x independent variable
    * @return the differential value (i.e. derivative value)
    */
  override def value(x: DerivativeStructure): DerivativeStructure =
    x.subtract(quadraticVertexB).pow(2).multiply(quadraticVertexA).add(quadraticVertexC)

  /**
    * <p>The method {{{integrate(x: Double)}}} is used to
    * get value of analytical integral function.</p>
    *
    * @param x independent variable
    * @return integral value of analytical integral function.
    */
  override def integrate(x: Double): Double = quadraticVertexA * pow(quadraticVertexB, 2.0) * x +
    quadraticVertexC * x - quadraticVertexA * quadraticVertexB * Math.pow(x, 2.0) +
    quadraticVertexA * pow(x, 3) / 3.0

  /**
    * <p>the method {{{derivative(x: Double)}}} is used to get the derivative value of
    * function at specified {{{x}}}.
    *
    * @param x independent variable
    * @return the derivative value of analytical derivative function.
    */
  override def derivative(x: Double): Double = 2 * quadraticVertexA * (x - quadraticVertexB)

  /**
    * The add operation for {{{QuadraticVertex}}} object and  a double number.
    *
    * @param n the operands
    * @return new { @code QuadraticVertex} object
    */
  def +(n: Double): TQuadraticVertex = {
    new QuadraticVertex(quadraticVertexA, quadraticVertexB, quadraticVertexC + n)
  }

  /**
    * The subtraction operation for {{{QuadraticVertex}}} object and  a double number.
    *
    * @param n the operands
    * @return new { @code QuadraticVertex} object
    */
  def -(n: Double): TQuadraticVertex = {
    new QuadraticVertex(quadraticVertexA, quadraticVertexB, quadraticVertexC - n)
  }

  /**
    * The multiplication operation for {{{QuadraticVertex}}} object and  a double number.
    *
    * @param n the operands
    * @return new { @code QuadraticVertex} object
    */
  def *(n: Double): TQuadraticVertex = {
    new QuadraticVertex(quadraticVertexA * n, quadraticVertexB, quadraticVertexC * n)
  }

  /**
    * The division operation for {{{QuadraticVertex}}} object and  a double number.
    *
    * @param n the operands
    * @return new { @code QuadraticVertex} object
    */
  def /(n: Double): TQuadraticVertex = {
    new QuadraticVertex(quadraticVertexA / n, quadraticVertexB, quadraticVertexC / n)
  }

  /**
    * whether {{{other}}} is equals {{{this}}}
    *
    * @param other another instance of class [[TQuadraticVertex]]
    * @return {{{Boolean}}} for whether {{{other}}} is equals {{{this}}}
    */
  override def equals(other: Any): Boolean = other match {
    case that: TQuadraticVertex =>
      (that canEqual this) &&
        quadraticVertexA == that.quadraticVertexA &&
        quadraticVertexB == that.quadraticVertexB &&
        quadraticVertexC == that.quadraticVertexC
    case _ => false
  }

  /**
    * whether {{{other}}} is instance of class [[TQuadraticVertex]]
    *
    * @param other another instance of class [[TQuadraticVertex]]
    * @return {{{Boolean}}} for whether {{{other}}} is instance of class [[TQuadraticVertex]]
    */
  def canEqual(other: Any): Boolean = other.isInstanceOf[TQuadraticVertex]

  /**
    * to get the hash code.it was used for method {{{equals(other: Any)}}}
    *
    * @return
    */
  override def hashCode(): Int = {
    val state = Seq(quadraticVertexA, quadraticVertexB, quadraticVertexC)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

/**
  * Companion Object for class [[TQuadraticVertex]].
  */
object TQuadraticVertex {

  final class Parametric extends ParametricUnivariateFunction {
    override def value(x: Double, parameters: Double*): Double = {
      checkParameter(parameters: _*)
      val a = parameters(0)
      val b = parameters(1)
      val c = parameters(2)
      a * pow(x - b, 2) + c
    }

    override def gradient(x: Double, parameters: Double*): Array[Double] = {
      checkParameter(parameters: _*)
      val a = parameters(0)
      val b = parameters(1)
      val c = parameters(2)
      val result = Array[Double](3)
      result(0) = pow(x - b, 2)
      result(1) = -2 * a * (x - b)
      result(2) = 1
      result
    }

    def checkParameter(parameters: Double*): Unit = {
      if (parameters == null) throw new IllegalArgumentException(s"Expected the parameter {parameters != null},but got {parameters = null}}")
      if (parameters.length != 3) throw new IllegalArgumentException(s"Expected the parameter {parameters.length == 3},but got {parameters.length = ${parameters.length}}")
      if (parameters.head == 0) throw new IllegalArgumentException(s"Expected the parameter {parameters(0) != 0},but got {parameters(0) = ${parameters.head}}")
    }
  }

  /**
    * in order to acquire ability of constructing [[TQuadraticVertex]] object without new.
    */
  def apply(quadraticVertexA: Double = 1.0, quadraticVertexB: Double = 0.0, quadraticVertexC: Double = 0.0): TQuadraticVertex =
    new QuadraticVertex(quadraticVertexA, quadraticVertexB, quadraticVertexC)

  /**
    * in order to acquire ability of case information.
    */
  def unapply(qv: TQuadraticVertex): Option[(Double, Double, Double)] = {
    if (qv == null) {
      None
    } else {
      Some(qv.quadraticVertexA, qv.quadraticVertexB, qv.quadraticVertexC)
    }
  }

  def quadraticVertex(quadraticVertexA: Double = 1.0, quadraticVertexB: Double = 0.0, quadraticVertexC: Double = 0.0)(x: Double): Double = quadraticVertexA * pow(x - quadraticVertexB, 2) + quadraticVertexC

  def quadraticVertexDerivative(quadraticVertexA: Double = 1.0, quadraticVertexB: Double = 0.0, quadraticVertexC: Double = 0.0)(x: Double): Double = 2 * quadraticVertexA * (x - quadraticVertexB)

  def quadraticVertexIntegrate(quadraticVertexA: Double = 1.0, quadraticVertexB: Double = 0.0, quadraticVertexC: Double = 0.0)(x: Double): Double = quadraticVertexA * pow(quadraticVertexB, 2.0) * x +
    quadraticVertexC * x - quadraticVertexA * quadraticVertexB * pow(x, 2) + quadraticVertexA * pow(x, 3) / 3

  def quadraticVertexDerivativeA(quadraticVertexA: Double = 1.0, quadraticVertexB: Double = 0.0, quadraticVertexC: Double = 0.0)(x: Double): Double = pow(x - quadraticVertexB, 2)

  def quadraticVertexDerivativeB(quadraticVertexA: Double = 1.0, quadraticVertexB: Double = 0.0, quadraticVertexC: Double = 0.0)(x: Double): Double = -2 * quadraticVertexA * (x - quadraticVertexB)

  def quadraticVertexDerivativeC(quadraticVertexA: Double = 1.0, quadraticVertexB: Double = 0.0, quadraticVertexC: Double = 0.0)(x: Double): Double = 1
}