package cn.edu.gxust.jiweihuang.scala.math.function.univariate


final case class QuadraticVertexLogistic(override val quadraticVertexA: Double = 1.0,
                                         override val quadraticVertexB: Double = 0.0,
                                         override val quadraticVertexC: Double = 0.0,
                                         override val logisticM: Double = 1.0,
                                         override val logisticK: Double = 1.0,
                                         override val logisticX0: Double = 0.0,
                                         override val quadraticVertexLogisticD: Double = 0.0)
  extends TQuadraticVertexLogistic