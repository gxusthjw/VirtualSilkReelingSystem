package cn.edu.gxust.jiweihuang.scala.math.fitting

import java.util

import cn.edu.gxust.jiweihuang.scala.math.function.univariate.QuadraticVertex
import org.hipparchus.fitting.{AbstractCurveFitter, WeightedObservedPoint}
import org.hipparchus.optim.nonlinear.vector.leastsquares.LeastSquaresProblem

class QuadraticVertexFitter(val initialGuess: Array[Double], val maxIter: Int) extends AbstractCurveFitter{
  val FUNCTION: QuadraticVertex.Parametric = new QuadraticVertex.Parametric()
  override def getProblem(points: util.Collection[WeightedObservedPoint]): LeastSquaresProblem = ???
}