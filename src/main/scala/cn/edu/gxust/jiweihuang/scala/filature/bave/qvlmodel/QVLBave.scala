package cn.edu.gxust.jiweihuang.scala.filature.bave.qvlmodel

import org.hipparchus.analysis.ParametricUnivariateFunction

final case class QVLBave(override val baveID: Long,
                         override val modelBaveLength: Double,
                         override val initialSize: Double,
                         override val terminalSize: Double,
                         override val maxSizePos: Double)
  extends TQVLBave

object QVLBave {

  final class Parametric extends ParametricUnivariateFunction {
    override def value(x: Double, parameters: Double*): Double = {
      checkParameter(parameters: _*)
      QVLBave(1, parameters(0), parameters(1), parameters(2), parameters(3)).value(x)
    }

    override def gradient(x: Double, parameters: Double*): Array[Double] = {
      checkParameter(parameters: _*)
      val modelBaveLength: Double = parameters(0)
      val initialSize: Double = parameters(1)
      val terminalSize: Double = parameters(2)
      val maxSizePos: Double = parameters(3)
      val result = new Array[Double](3)
      result
    }

    def checkParameter(parameters: Double*): Unit = {
      if (parameters == null) throw new IllegalArgumentException(s"Expected the parameter {parameters != null},but got {parameters = null}}")
      if (parameters.length != 4) throw new IllegalArgumentException(s"Expected the parameter {parameters.length == 4},but got {parameters.length = ${parameters.length}}")
      if (parameters.head == 0) throw new IllegalArgumentException(s"Expected the parameter {parameters(0) != 0},but got {parameters(0) = ${parameters.head}}")
    }
  }

}