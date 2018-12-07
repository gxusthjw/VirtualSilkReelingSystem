package cn.edu.gxust.jiweihuang.scala.math.function.univariate

/**
  * The trait [[TQuadratic]] is used for
  * representing quadratic function.
  *
  * @see TUnivariateDerivativeFunction
  * @see TUnivariateDifferentiableFunction
  * @see TUnivariateIntegrableFunction
  * @see TUnivariateIntegrateFunction
  */
trait TQuadratic extends TUnivariateDifferentiableFunction
  with TUnivariateIntegrableFunction
  with TUnivariateDerivativeFunction
  with TUnivariateIntegralFunction {
  /**
    * The vertex coordinates (x,y) of quadratic function
    */
  val vertex: Array[Double]
  /**
    * <p>whether quadratic function is invert.</p>
    * <p>if invert (i.e. open side down) return true else return false.</p>
    */
  val isInvert: Boolean
  /**
    * the x coordinate of intersection with x axis of function
    */
  val xIntersection: Array[Double]
  /**
    * the y coordinate of intersection with y axis of function
    */
  val yIntersection: Double
  /**
    * The number of intersection with x axis of function
    */
  val xIntersectionNum: Int
}
