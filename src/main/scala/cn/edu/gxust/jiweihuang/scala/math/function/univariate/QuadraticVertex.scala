package cn.edu.gxust.jiweihuang.scala.math.function.univariate

final case class QuadraticVertex(override val quadraticVertexA: Double = 1.0,
                                 override val quadraticVertexB: Double = 0.0,
                                 override val quadraticVertexC: Double = 0.0)
  extends TQuadraticVertex
