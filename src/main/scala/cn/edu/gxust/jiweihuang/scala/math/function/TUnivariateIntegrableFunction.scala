package cn.edu.gxust.jiweihuang.scala.math.function

import org.hipparchus.analysis.integration._

trait TUnivariateIntegrableFunction extends TUnivariateFunction {

  private[this] val DefaultIntegrationPointsNumber = 32

  /**
    * <p>to get the definite integral value of univariate function
    * with Romberg algorithm</p>
    *
    * @param relativeAccuracy        relative accuracy of the result
    * @param absoluteAccuracy        absolute accuracy of the result
    * @param minimalIterationCount   minimum number of iterations
    * @param maximalIterationCount   maximum number of iterations
    * @param maxIter                 Maximum number of evaluations.
    * @param lowerX                  the lower limit of integral.
    * @param upperX                  the upper limit of integral.
    * @return the definite integral value.
    */
  def integrateRomberg(relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                       absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                       minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                       maximalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MAX_ITERATIONS_COUNT,
                       maxIter: Int = Int.MaxValue,
                       lowerX: Double = lowerX,
                       upperX: Double = upperX): Double =
    new RombergIntegrator(relativeAccuracy, absoluteAccuracy,
      minimalIterationCount, maximalIterationCount).integrate(maxIter, this, lowerX, upperX)

  /**
    * <p>to get the definite integral value of univariate function
    * with Simpson algorithm</p>
    *
    * @param relativeAccuracy        relative accuracy of the result
    * @param absoluteAccuracy        absolute accuracy of the result
    * @param minimalIterationCount   minimum number of iterations
    * @param maximalIterationCount   maximum number of iterations
    * @param maxIter                 Maximum number of evaluations.
    * @param lowerX                  the lower limit of integral.
    * @param upperX                  the upper limit of integral.
    * @return the definite integral value.
    */
  def integrateSimpson(relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                       absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                       minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                       maximalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MAX_ITERATIONS_COUNT,
                       maxIter: Int = Int.MaxValue,
                       lowerX: Double = lowerX,
                       upperX: Double = upperX): Double =
    new SimpsonIntegrator(relativeAccuracy, absoluteAccuracy,
      minimalIterationCount, maximalIterationCount).integrate(maxIter, this, lowerX, upperX)

  /**
    * <p>to get the definite integral value of univariate function
    * with MidPoint algorithm</p>
    *
    * @param relativeAccuracy        relative accuracy of the result
    * @param absoluteAccuracy        absolute accuracy of the result
    * @param minimalIterationCount   minimum number of iterations
    * @param maximalIterationCount   maximum number of iterations
    * @param maxIter                 Maximum number of evaluations.
    * @param lowerX                  the lower limit of integral.
    * @param upperX                  the upper limit of integral.
    * @return the definite integral value.
    */
  def integrateMidPoint(relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                        absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                        minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                        maximalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MAX_ITERATIONS_COUNT,
                        maxIter: Int = Int.MaxValue,
                        lowerX: Double = lowerX,
                        upperX: Double = upperX): Double =
    new MidPointIntegrator(relativeAccuracy, absoluteAccuracy,
      minimalIterationCount, maximalIterationCount).integrate(maxIter, this, lowerX, upperX)

  /**
    * <p>to get the definite integral value of univariate function
    * with Trapezoid algorithm</p>
    *
    * @param relativeAccuracy        relative accuracy of the result
    * @param absoluteAccuracy        absolute accuracy of the result
    * @param minimalIterationCount   minimum number of iterations
    * @param maximalIterationCount   maximum number of iterations
    * @param maxIter                 Maximum number of evaluations.
    * @param lowerX                  the lower limit of integral.
    * @param upperX                  the upper limit of integral.
    * @return the definite integral value.
    */
  def integrateTrapezoid(relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                         absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                         minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                         maximalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MAX_ITERATIONS_COUNT,
                         maxIter: Int = Int.MaxValue,
                         lowerX: Double = lowerX,
                         upperX: Double = upperX): Double =
    new TrapezoidIntegrator(relativeAccuracy, absoluteAccuracy,
      minimalIterationCount, maximalIterationCount).integrate(maxIter, this, lowerX, upperX)

  /**
    * <p>to get the definite integral value of univariate function
    * with IterativeLegendreGauss algorithm</p>
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
    */
  def integrateIterativeLegendreGauss(relativeAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_RELATIVE_ACCURACY,
                                      absoluteAccuracy: Double = BaseAbstractUnivariateIntegrator.DEFAULT_ABSOLUTE_ACCURACY,
                                      minimalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MIN_ITERATIONS_COUNT,
                                      maximalIterationCount: Int = BaseAbstractUnivariateIntegrator.DEFAULT_MAX_ITERATIONS_COUNT,
                                      integrationPointsNumber: Int = DefaultIntegrationPointsNumber,
                                      maxIter: Int = Int.MaxValue,
                                      lowerX: Double = lowerX,
                                      upperX: Double = upperX): Double =
    new IterativeLegendreGaussIntegrator(integrationPointsNumber,
      relativeAccuracy, absoluteAccuracy, minimalIterationCount,
      maximalIterationCount).integrate(maxIter, this, lowerX, upperX)
}

