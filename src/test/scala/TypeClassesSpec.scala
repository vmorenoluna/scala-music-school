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
    notePrimitiveInstance.eqv(Note(qn, (C, 4)), Note(qn, (C, 4))) shouldEqual true
    notePrimitiveInstance.neqv(Note(qn, (C, 4)), Note(en, (D, 5))) shouldEqual true
    restPrimitiveInstance.eqv(Rest(qn), Rest(qn)) shouldEqual true
    restPrimitiveInstance.neqv(Rest(qn), Rest(en)) shouldEqual true
  }

  "Primitive" should "be in Order type class" in {
    notePrimitiveInstance.compare(Note(qn, (E, 4)), Note(qn, (C, 4))) shouldEqual 1
    notePrimitiveInstance.compare(Note(qn, (C, 4)), Note(en, (C, 4))) shouldEqual 1
    notePrimitiveInstance.compare(Note(qn, (C, 4)), Note(qn, (C, 4))) shouldEqual 0
    notePrimitiveInstance.compare(Note(qn, (F, 4)), Note(en, (D, 5))) shouldEqual -1
    notePrimitiveInstance.compare(Note(qn, (D, 4)), Note(hn, (D, 5))) shouldEqual -1
    restPrimitiveInstance.compare(Rest(wn), Rest(qn)) shouldEqual 1
    restPrimitiveInstance.compare(Rest(qn), Rest(qn)) shouldEqual 0
    restPrimitiveInstance.compare(Rest(en), Rest(qn)) shouldEqual -1
  }

}
