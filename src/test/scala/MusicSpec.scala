import music.Music._
import music.Types._
import music._

class MusicSpec extends UnitSpec {

  "line" should "create a sequential Music from a list of notes" in {
    val notes: List[Music[Pitch]] = List(c(4)(qn), d(4)(qn), e(4)(qn), fs(4)(qn), gs(4)(qn))

    line(notes) should equal(
      c(4)(qn) :+: d(4)(qn) :+: e(4)(qn) :+: fs(4)(qn) :+: gs(4)(qn) :+: rest(0)
    )
  }

  "chord" should "create a parallel Music from a list of notes" in {
    val notes: List[Music[Pitch]] = List(c(4)(qn), d(4)(qn), e(4)(qn), fs(4)(qn), gs(4)(qn))

    chord(notes) should equal(
        c(4)(qn) :=:
        d(4)(qn) :=:
        e(4)(qn) :=:
        fs(4)(qn) :=:
        gs(4)(qn) :=:
        rest(0)
    )
  }

  "maxPitch" should "return the highest pitch from a list of pitches" in {
    val pitches: List[Pitch] = List((C,4), (D,2), (E,7), (F,5))

    maxPitch(pitches) should equal((E,7))
  }

  "maxAbsPitch" should "return the highest absolute pitch from a list of absolute pitches" in {
    val pitches: List[AbsPitch] = List(50, 68, 72, 59)

    maxAbsPitch(pitches) should equal(72)
  }

  "minAbsPitch" should "return the lowest absolute pitch from a list of absolute pitches" in {
    val pitches: List[AbsPitch] = List(50, 68, 72, 59)

    minAbsPitch(pitches) should equal(50)
  }

  "times" should "repeat a music the specified amount of times" in {
    val m: Music[Pitch] = c(4)(qn) :+: d(4)(qn) :+: e(4)(qn)
    val n: Int = 2

    times(n, m) should equal(
      (c(4)(qn) :+: d(4)(qn) :+: e(4)(qn)) :+: ((c(4)(qn) :+: d(4)(qn) :+: e(4)(qn)) :+: rest(0))
    )
  }

  "addDuration" should "add a duration to a list of notes" in {
    val notes: List[Duration => Music[Pitch]] = List(c(4), d(4), e(4))
    val duration: Duration = qn

    addDuration(duration, notes) should equal(
      c(4)(qn) :+: d(4)(qn) :+: e(4)(qn) :+: rest(0)
    )
  }

  "graceNote" should "add a grace note to a note" in {
    val note: Music[Pitch] = e(4)(qn)

    graceNote(ws, note) should equal(
      fs(4)(qn/8) :+: e(4)(7*qn/8)
    )
  }

}