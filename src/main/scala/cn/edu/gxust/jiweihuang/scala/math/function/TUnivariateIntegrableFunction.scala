package cn.edu.gxust.jiweihuang.scala.math.function

import org.hipparchus.analysis.integration._

trait TUnivariateIntegrableFunction extends TUnivariateFunction {

  /**
    * <p>The method is used to get the definite integral value
    * of univariate function with Romberg algorithm</p>
    *
    * @param relativeAccuracy      relative accuracy of the result
    * @param absoluteAccuracy      absolute accuracy of the result
    * @param minimalIterationCount minimum number of iterations
    * @param maximalIterationCount maximum number of iterations
    * @param maxIter               Maximum number of evaluations.
    * @param lowerX                the lower limit of integral.
    * @param upperX                the upper limit of integral.
    * @return the definite integral value.
    * @see RombergIntegrator
    */
  def integrateRomberg(lowerX: Double, upperX: Double, maxIter: Int = Int.MaxValue,
                       relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                       absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                       minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                       maximalIterationCount: Int = RombergIntegrator.ROMBERG_MAX_ITERATIONS_COUNT
                      ): Double =
    new RombergIntegrator(relativeAccuracy, absoluteAccuracy,
      minimalIterationCount, maximalIterationCount).integrate(maxIter, this, lowerX, upperX)

  /**
    * <p>The method is used to get the definite integral value
    * of univariate function with Simpson algorithm</p>
    *
    * @param relativeAccuracy      relative accuracy of the result
    * @param absoluteAccuracy      absolute accuracy of the result
    * @param minimalIterationCount minimum number of iterations
    * @param maximalIterationCount maximum number of iterations
    * @param maxIter               Maximum number of evaluations.
    * @param lowerX                the lower limit of integral.
    * @param upperX                the upper limit of integral.
    * @return the definite integral value.
    * @see SimpsonIntegrator
    */
  def integrateSimpson(lowerX: Double, upperX: Double, maxIter: Int = Int.MaxValue,
                       relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                       absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                       minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                       maximalIterationCount: Int = SimpsonIntegrator.SIMPSON_MAX_ITERATIONS_COUNT
                      ): Double =
    new SimpsonIntegrator(relativeAccuracy, absoluteAccuracy,
      minimalIterationCount, maximalIterationCount).integrate(maxIter, this, lowerX, upperX)

  /**
    * <p>The method is used to get the definite integral value
    * of univariate function with MidPoint algorithm</p>
    *
    * @param relativeAccuracy      relative accuracy of the result
    * @param absoluteAccuracy      absolute accuracy of the result
    * @param minimalIterationCount minimum number of iterations
    * @param maximalIterationCount maximum number of iterations
    * @param maxIter               Maximum number of evaluations.
    * @param lowerX                the lower limit of integral.
    * @param upperX                the upper limit of integral.
    * @return the definite integral value.
    * @see MidPointIntegrator
    */
  def integrateMidPoint(lowerX: Double, upperX: Double, maxIter: Int = Int.MaxValue,
                        relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                        absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                        minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                        maximalIterationCount: Int = MidPointIntegrator.MIDPOINT_MAX_ITERATIONS_COUNT
                       ): Double =
    new MidPointIntegrator(relativeAccuracy, absoluteAccuracy,
      minimalIterationCount, maximalIterationCount).integrate(maxIter, this, lowerX, upperX)

  /**
    * <p>The method is used to get the definite integral value
    * of univariate function with Trapezoid algorithm</p>
    *
    * @param relativeAccuracy      relative accuracy of the result
    * @param absoluteAccuracy      absolute accuracy of the result
    * @param minimalIterationCount minimum number of iterations
    * @param maximalIterationCount maximum number of iterations
    * @param maxIter               Maximum number of evaluations.
    * @param lowerX                the lower limit of integral.
    * @param upperX                the upper limit of integral.
    * @return the definite integral value.
    * @see TrapezoidIntegrator
    */
  def integrateTrapezoid(lowerX: Double, upperX: Double, maxIter: Int = Int.MaxValue,
                         relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                         absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                         minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                         maximalIterationCount: Int = TrapezoidIntegrator.TRAPEZOID_MAX_ITERATIONS_COUNT
                        ): Double =
    new TrapezoidIntegrator(relativeAccuracy, absoluteAccuracy,
      minimalIterationCount, maximalIterationCount).integrate(maxIter, this, lowerX, upperX)


  /**
    * <p>The method is used to get the definite integral value
    * of univariate function with IterativeLegendreGauss algorithm</p>
    *
    * @param relativeAccuracy        relative accuracy of the result
    * @param absoluteAccuracy        absolute accuracy of the result
    * @param minimalIterationCount   minimum number of iterations
    * @param maximalIterationCount   maximum number of iterations
    * @param integrationPointsNumber Number of integration points.
    * @param maxIter                 Maximum number of evaluations.
    * @param lowerX                  the lower limit of integral.
    * @param upperX                  the upper limit of integral.
    * @return the definite integral value.
    * @see IterativeLegendreGaussIntegrator
    */
  def integrateIterativeLegendreGauss(lowerX: Double, upperX: Double, maxIter: Int = Int.MaxValue,
                                      integrationPointsNumber: Int = DefaultIntegrationPointsNumber,
                                      relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                                      absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                                      minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                                      maximalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MAX_ITERATIONS_COUNT
                                     ): Double =
    new IterativeLegendreGaussIntegrator(integrationPointsNumber,
      relativeAccuracy, absoluteAccuracy, minimalIterationCount,
      maximalIterationCount).integrate(maxIter, this, lowerX, upperX)

  private[this] val DefaultIntegrationPointsNumber = 32
}

