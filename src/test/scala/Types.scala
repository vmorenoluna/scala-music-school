import music._
import music.Types._
import org.scalatest.prop.Generator

class Types extends UnitSpec {

  val absPitchGen: Generator[AbsPitch] = posIntValues
  val pitchClassGen: Generator[PitchClass] = specificValues(C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B)
  val octaveGen: Generator[Octave] = intsBetween(0, 10)
  def pitchGen: Generator[Pitch] = tuple2s(pitchClassGen, octaveGen)

  "pitch (ap)" should "return a valid pitch for each positive absolute pitch" in {
    forAll (absPitchGen) { absolutePitch =>
        whenever (absolutePitch > 0) {
          val p: Pitch = pitch(absolutePitch)
          val pitchClass: PitchClass = p._1
          val octave: Octave = p._2

          List(C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B) should contain oneElementOf List(pitchClass)
          octave should be >= 0
        }
    }
  }

  "absPitch (pitch)" should "return a valid absolute pitch for each pitch" in {
    forAll (pitchGen) { pitch =>
      absPitch(pitch) should be >= 0
    }
  }

  "absPitch (pitch ap)" should "return ap" in {
    forAll (absPitchGen) { absolutePitch =>
      whenever (absolutePitch > 0) {
        absPitch(pitch(absolutePitch)) shouldEqual absolutePitch
      }
    }
  }

}
