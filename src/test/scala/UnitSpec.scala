import music.Types.{AbsPitch, Octave, Pitch}
import music.{A, As, B, C, Cs, D, Ds, E, F, Fs, G, Gs, PitchClass}
import org.scalatest.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.{Generator, GeneratorDrivenPropertyChecks}

abstract class UnitSpec extends AnyFlatSpec with GeneratorDrivenPropertyChecks with Matchers {

  val pitchClassGen: Generator[PitchClass] = specificValues(C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B)
  val absPitchGen: Generator[AbsPitch] = posIntValues
  val octaveGen: Generator[Octave] = intsBetween(0, 10)
  def pitchGen: Generator[Pitch] = tuple2s(pitchClassGen, octaveGen)

}
