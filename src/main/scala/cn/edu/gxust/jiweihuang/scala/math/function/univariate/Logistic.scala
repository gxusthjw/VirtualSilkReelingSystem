package cn.edu.gxust.jiweihuang.scala.math.function.univariate

final case class Logistic(override val logisticM: Double = 1.0,
                          override val logisticK: Double = -1.0,
                          override val logisticX0: Double = 0.0)
  extends TLogistic
