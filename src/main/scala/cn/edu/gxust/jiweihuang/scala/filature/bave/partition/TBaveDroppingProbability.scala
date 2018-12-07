package cn.edu.gxust.jiweihuang.scala.filature.bave.partition

trait TBaveDroppingProbability {
  def droppingProbability(pos: Double): Double

  def notDroppingProbability(pos: Double): Double = 1 - droppingProbability(pos)
}
