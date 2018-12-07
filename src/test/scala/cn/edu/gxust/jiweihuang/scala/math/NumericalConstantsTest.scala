package cn.edu.gxust.jiweihuang.scala.math

import cn.edu.gxust.jiweihuang.scala.test.UnitSpec

class NumericalConstantsTest extends UnitSpec {
  "NumericalConstants.NumericalPrecision" should " equals to 1e-15" in {
    println(s"NumericalConstants.NumericalPrecision = ${NumericalConstants.NumericalPrecision}")
    assertResult(1e-15)(NumericalConstants.NumericalPrecision)
  }
}
