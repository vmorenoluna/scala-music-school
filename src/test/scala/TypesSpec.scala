import music.Types._
import music._

class TypesSpec extends UnitSpec {

  "pitch (ap)" should "return a valid pitch for each positive absolute pitch" in {
    forAll (absPitchGen) { absolutePitch =>
        whenever (absolutePitch > 0) {
          val p: Pitch = pitch(absolutePitch)
          val pitchClass: PitchClass = p._1
          val octave: Octave = p._2

          List(C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B) should contain (pitchClass)
          octave should be >= 0
        }
    }
  }

  "absPitch (p)" should "return a valid absolute pitch for each pitch" in {
    forAll (pitchGen) { pitch =>
      absPitch(pitch) should be >= 0
    }
  }

  "absPitch (pitch ap)" should "return the original absolute pitch" in {
    forAll (absPitchGen) { absolutePitch =>
      whenever (absolutePitch > 0) {
        absPitch(pitch(absolutePitch)) shouldEqual absolutePitch
      }
    }
  }

  "pitch (absPitch p)" should "return the original pitch" in {
    forAll (pitchGen) { p =>
        pitch(absPitch(p)) shouldEqual p
    }
  }

}
