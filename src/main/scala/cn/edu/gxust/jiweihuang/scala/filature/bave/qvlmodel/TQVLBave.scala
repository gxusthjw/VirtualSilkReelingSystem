package cn.edu.gxust.jiweihuang.scala.filature.bave.qvlmodel

import cn.edu.gxust.jiweihuang.scala.filature.bave.TBave
import cn.edu.gxust.jiweihuang.scala.math.function.univariate.TQuadraticVertexLogistic
import math._

trait TQVLBave extends TBave with TQuadraticVertexLogistic {

  val modelBaveLength: Double
  val initialSize: Double
  val terminalSize: Double
  val maxSizePos: Double

  override val baveLength: Double = modelBaveLength

  override def baveSize(pos: Double): Unit = value(pos)

  override val quadraticVertexA: Double = initialSize * (1 + exp(logisticK * maxSizePos)) * (1 - terminalSize/initialSize) / pow(modelBaveLength,2)
  override val quadraticVertexB: Double = modelBaveLength
  override val quadraticVertexC: Double = 0
  override val logisticM: Double = 1
  override val logisticK: Double = 4 / (modelBaveLength - maxSizePos)
  override val logisticX0: Double = maxSizePos
  override val quadraticVertexLogisticD: Double = terminalSize
}
