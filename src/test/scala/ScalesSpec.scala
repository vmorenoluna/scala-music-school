import music._
import music.Music._
import music.Scales._
import music.Types._

class ScalesSpec extends UnitSpec {

  "wholeToneScale (p)" should "return the correct whole tone scale" in {
    val root: Pitch = (C, 4)

    wholeToneScale(root) should equal(List(c(4)(qn), d(4)(qn), e(4)(qn), fs(4)(qn), gs(4)(qn)))
  }

  "chromaticScale (ap1,ap2)" should "return the correct chromatic scale from ap1 to ap2" in {
    val ap1: AbsPitch = absPitch((C, 4))
    val ap2: AbsPitch = absPitch((E, 4))

    chromaticScale(ap1, ap2) should equal(List(c(4)(qn), cs(4)(qn), d(4)(qn), ds(4)(qn), e(4)(qn)))
  }

  "chromaticScale (p1,p2)" should "return the correct ascending chromatic scale from p1 to p2" in {
    val p1: Pitch = (C, 4)
    val p2: Pitch = (E, 4)

    chromaticScale(p1, p2) should equal(
      c(4)(qn) :+: cs(4)(qn) :+: d(4)(qn) :+: ds(4)(qn) :+: e(4)(qn) :+: rest(0)
    )
    // c :+: cs :+: d = d.:+:(c :+: cs) = :+:(c :+: cs, d) = :+:(cs.:+:(c), d) = :+:(:+:(c, cs), d)
  }

  "makeScale (pitch, pattern)" should "return the correct scale" in {
    val pitch: Pitch = (C, 4)
    val pattern: List[Step] = List(ws, ws, hs, ws, ws, ws)

    makeScale(pitch, pattern) should equal(
      c(4)(qn) :+: d(4)(qn) :+: e(4)(qn) :+: f(4)(qn) :+: g(4)(qn) :+: music.Music.a(4)(qn) :+: b(4)(qn) :+: rest(0)
    )
  }

  "genScale (mode)" should "return the correct scale pattern" in {
    genScale(Major) should equal(List(ws, ws, hs, ws, ws, ws, hs))
    genScale(Ionian) should equal(List(ws, ws, hs, ws, ws, ws, hs))
    genScale(Dorian) should equal(List(ws, hs, ws, ws, ws, hs, ws))
    genScale(Phrygian) should equal(List(hs, ws, ws, ws, hs, ws, ws))
    genScale(Lydian) should equal(List(ws, ws, ws, hs, ws, ws, hs))
    genScale(Mixolydian) should equal(List(ws, ws, hs, ws, ws, hs, ws))
    genScale(Minor) should equal(List(ws, hs, ws, ws, hs, ws, ws))
    genScale(Aeolian) should equal(List(ws, hs, ws, ws, hs, ws, ws))
    genScale(Locrian) should equal(List(hs, ws, ws, hs, ws, ws, ws))
  }

  private def verifyPattern(scale: Music[Pitch], pattern: List[Step]): Boolean = ??? // TODO it needs an implementation for :+: and :=:

}