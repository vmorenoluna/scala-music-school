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
    primitiveInstance.eqv(Note(qn, (C, 4)), Note(qn, (C, 4))) shouldEqual true
    primitiveInstance.neqv(Note(qn, (C, 4)), Note(en, (D, 5))) shouldEqual true
    primitiveInstance.eqv(Rest(qn), Rest(qn)) shouldEqual true
    primitiveInstance.neqv(Rest(qn), Rest(en)) shouldEqual true
  }

  "Primitive" should "be in Order type class" in {
    primitiveInstance.compare(Note(qn, (E, 4)), Note(qn, (C, 4))) shouldEqual 1
    primitiveInstance.compare(Note(qn, (C, 4)), Note(en, (C, 4))) shouldEqual 1
    primitiveInstance.compare(Note(qn, (C, 4)), Note(qn, (C, 4))) shouldEqual 0
    primitiveInstance.compare(Note(qn, (F, 4)), Note(en, (D, 5))) shouldEqual -1
    primitiveInstance.compare(Note(qn, (D, 4)), Note(hn, (D, 5))) shouldEqual -1
    primitiveInstance.compare(Rest(wn), Rest(qn)) shouldEqual 1
    primitiveInstance.compare(Rest(qn), Rest(qn)) shouldEqual 0
    primitiveInstance.compare(Rest(en), Rest(qn)) shouldEqual -1
  }

}
