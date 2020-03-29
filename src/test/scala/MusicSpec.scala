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

  "lineToList" should "create a list of notes from a sequential Music made with line" in {
    val music = c(4)(qn) :+: d(4)(qn) :+: e(4)(qn) :+: fs(4)(qn) :+: gs(4)(qn) :+: rest(0)

    lineToList(music) should equal(
      List(c(4)(qn), d(4)(qn), e(4)(qn), fs(4)(qn), gs(4)(qn))
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
    val pitches: List[Pitch] = List((C, 4), (D, 2), (E, 7), (F, 5))

    maxPitch(pitches) should equal((E, 7))
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
      fs(4)(qn / 8) :+: e(4)(7 * qn / 8)
    )
  }

  "apPairsMusic" should "create a music from a list of absolute pitches pairs" in {
    lazy val apsList: LazyList[(AbsPitch, AbsPitch)] = (50, 50) #:: apsList.map(ps => (ps._1 + 1, ps._2 + 1))
    val apPairsList: List[(AbsPitch, AbsPitch)] = apsList.take(3).toList

    apPairsMusic(apPairsList) should equal(
      (d(4)(qn) :=: d(4)(qn)) :+:
        (ds(4)(en) :=: ds(4)(en)) :+:
        (e(4)(qn) :=: e(4)(qn)) :+:
        rest(0)
    )
  }

  "offset" should "prepend a rest to the music" in {
    val duration = wn
    val music = c(4)(qn)

    offset(duration, music) should equal(
      rest[Pitch](duration) :+: music
    )
  }

  "invert" should "invert a line" in {
    val music = b(4)(qn) :+: f(5)(qn) :+: g(4)(qn) :+: c(5)(qn)

    invert(music) should equal(
      b(4)(qn) :+: f(4)(qn) :+: ds(5)(qn) :+: as(4)(qn) :+: rest(0)
    )
  }

  "retro" should "reverse a line" in {
    val music = b(4)(qn) :+: f(5)(qn) :+: g(4)(qn) :+: c(5)(qn)

    retro(music) should equal(
      c(5)(qn) :+: g(4)(qn) :+: f(5)(qn) :+: b(4)(qn) :+: rest(0)
    )
  }

  "retroInvert" should "reverse and invert a line" in {
    val music = b(4)(qn) :+: f(5)(qn) :+: g(4)(qn) :+: c(5)(qn)

    retroInvert(music) should equal(
      as(4)(qn) :+: ds(5)(qn) :+: f(4)(qn) :+: b(4)(qn) :+: rest(0)
    )
  }

  "invertRetro" should "invert and reverse a line" in {
    val music = b(4)(qn) :+: f(5)(qn) :+: g(4)(qn) :+: c(5)(qn)

    invertRetro(music) should equal(
      c(5)(qn) :+: f(5)(qn) :+: g(4)(qn) :+: cs(5)(qn) :+: rest(0)
    )
  }

  "properRow" should "detect if a music have exactly 12 notes" in {
    val notProperMusic = b(4)(qn) :+: f(5)(qn) :+: g(4)(qn) :+: c(5)(qn)

    properRow(notProperMusic) should equal(false)

    val properMusic =
        b(4)(qn) :+: c(5)(qn) :+: cs(4)(qn) :+: d(5)(qn) :+:
        ds(5)(qn) :+: e(4)(qn) :+: f(5)(qn) :+: fs(5)(qn) :+:
        g(4)(qn) :+: gs(5)(qn) :+: gss(5)(qn) :+: as(4)(qn)

    properRow(properMusic) should equal(true)
  }

  "properRow" should "check each pitch class is used only once, regardless of the octave" in {
    val properMusic =
      b(4)(qn) :+: c(5)(qn) :+: c(4)(qn) :+: d(5)(qn) :+:
        ds(5)(qn) :+: e(4)(qn) :+: f(5)(qn) :+: fs(5)(qn) :+:
        g(4)(qn) :+: gs(5)(qn) :+: gss(7)(qn) :+: as(4)(qn)

    properRow(properMusic) should equal(false)
  }

  "properRow" should "ignore rests" in {
    val properMusic =
      b(4)(qn) :+: c(5)(qn) :+: cs(4)(qn) :+: d(5)(qn) :+: rest[Pitch](qn) :+:
        ds(5)(qn) :+: e(4)(qn) :+: f(5)(qn) :+: fs(5)(qn) :+: rest[Pitch](qn) :+:
        g(4)(qn) :+: gs(5)(qn) :+: gss(7)(qn) :+: as(4)(qn) :+: rest(0)

    properRow(properMusic) should equal(true)
  }

  "properRow" should "not consider as unique enharmonically equivalent pitch classes" in {
    val properMusic =
      b(4)(qn) :+: c(5)(qn) :+: cs(4)(qn) :+: df(5)(qn) :+:
        ds(5)(qn) :+: e(4)(qn) :+: f(5)(qn) :+: fs(5)(qn) :+:
        g(4)(qn) :+: gs(5)(qn) :+: gss(7)(qn) :+: as(4)(qn)

    properRow(properMusic) should equal(false)
  }

  "palin" should "detect a palindrome melody ignoring rests and durations" in {
    val music =
      b(4)(qn) :+: c(5)(qn) :+: cs(4)(en) :+: df(5)(qn) :+:
        rest[Pitch](qn) :+: f(2)(qn) :+: rest[Pitch](qn) :+:
        df(5)(qn) :+: cs(4)(qn) :+: c(5)(qn) :+: b(4)(wn)

    palin(music) should equal(true)
  }

  "retroPitches" should "reverse the pitches in a line while maintaining the durations order" in {
    val music = b(4)(en) :+: c(5)(qn) :+: cs(4)(hn) :+: df(5)(wn)

    retroPitches(music) should equal(
      df(5)(en) :+: cs(4)(qn) :+: c(5)(hn) :+: b(4)(wn) :+: rest(0)
    )
  }

}