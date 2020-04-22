import music.Music._
import music._
import music.PitchClass._
import music.PrimitiveTypeClassInstances._

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

  "Primitive" should "be in Eq type class" in {
    notePrimitive.eqv(Note(qn, (C, 4)), Note(qn, (C, 4))) shouldEqual true
    notePrimitive.neqv(Note(qn, (C, 4)), Note(en, (D, 5))) shouldEqual true
    restPrimitive.eqv(Rest(qn), Rest(qn)) shouldEqual true
    restPrimitive.neqv(Rest(qn), Rest(en)) shouldEqual true
  }

}
