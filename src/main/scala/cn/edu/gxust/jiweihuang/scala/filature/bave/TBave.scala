package cn.edu.gxust.jiweihuang.scala.filature.bave

trait TBave extends Serializable {
  val baveID:Long
  val baveLength:Double
  def baveSize(pos:Double)
}
