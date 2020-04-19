import music.{Cff, D}
import music.PitchClass.{pitchClassEq, pitchClassOrder}

class TypeClassesSpec extends UnitSpec {

  "PitchClass" should "be in Eq type class" in {
    pitchClassEq.eqv(Cff, Cff) shouldEqual true
    pitchClassEq.neqv(Cff, Cff) shouldEqual false
    pitchClassEq.eqv(Cff, D) shouldEqual false
    pitchClassEq.neqv(Cff, D) shouldEqual true
  }

  "PitchClass" should "be in Order type class" in {
    pitchClassOrder.compare(Cff, D) shouldEqual -1
    pitchClassOrder.compare(Cff, Cff) shouldEqual 0
    pitchClassOrder.compare(D, Cff) shouldEqual 1
  }

}
