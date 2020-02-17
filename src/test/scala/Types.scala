import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import music._
import music.Types._
import org.scalacheck.{Arbitrary, Gen}

class Types extends Specification with ScalaCheck {

  def pitchGen: Gen[Pitch] = for {
    pc <- Gen.oneOf(C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B)
    oct <- Gen.choose(0, 10)
  } yield (pc, oct)

  implicit def arbitraryPitch: Arbitrary[Pitch] = Arbitrary(pitchGen)

  "pitch (ap)" should {
    "return a valid pitch" in prop {
      (ap: AbsPitch) =>
        (ap > 0) ==> {
          val p: Pitch = pitch(ap)
          val pc: PitchClass = p._1
          val oct: Octave = p._2
          pc must beOneOf(C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B)
          oct must be_>=(0)
        }
    }
  }

  "absPitch (p)" should {
    "return a valid absolute pitch" in prop {
      (p: Pitch) => {
        absPitch(p) must beGreaterThanOrEqualTo (0)
      }
    }.setArbitrary(arbitraryPitch)
  }

  "absPitch (pitch ap)" should {
    "return ap" in prop {
      (ap: AbsPitch) =>
        (ap > 0) ==> {
          absPitch(pitch(ap)) mustEqual ap
        }
    }
  }

}
