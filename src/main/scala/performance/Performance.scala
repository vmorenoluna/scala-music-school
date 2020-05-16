package performance

import music.Music1.Note1
import music.{:+:, :=:, Custom, Instrument, KeySig, Modify, Music, Note, Phrase, Prim, Rest, Tempo, Transpose}
import music.Types.Duration
import performance.Performance.DurT
import performance.players.Player.players
import spire.math.Rational

object Performance {

  type Performance = List[MEvent]
  type PTime = Rational
  type DurT = Rational

  def perform(): Performance = ??? // TODO default perform

  def hsomPerform(c: Context[Note1], m: Music[Note1]): Performance =
    perf(c, m)._1

  def perf[A](c: Context[Note1], m: Music[Note1]): (Performance, DurT) = m match {
    case Prim(Note(d, p)) => (c.cPlayer.playNote(c)(d)(p), d * c.cDur)
    case Prim(Rest(d)) => (List.empty, d * c.cDur)
    case :+:(m1, m2) => {
      val (pf1, d1) = perf(c, m1)
      val (pf2, d2) = perf(c.copy(cTime = c.cTime + d1), m2)
      (pf1 ++ pf2, d1 + d2)
    }
    case :=:(m1, m2) => {
      val (pf1, d1) = perf(c, m1)
      val (pf2, d2) = perf(c, m2)
      (merge(pf1, pf2), d1 max d2)
    }
    case Modify(Tempo(r), m) => perf(c.copy(cDur = c.cDur / r), m)
    case Modify(Transpose(p), m) => perf(c.copy(cPch = c.cPch + p), m)
    case Modify(Instrument(i), m) => perf(c.copy(cInst = i), m)
    case Modify(KeySig(pc, mo), m) => perf(c.copy(cKey = (pc, mo)), m)
    case Modify(Phrase(pas), m) => c.cPlayer.interpPhrase(c, pas, m)
    case Modify(Custom(s), m) =>
      if (s.take(7) == "Player ") perf(c.copy(cPlayer = players.get(s.drop(7)).get), m)
      else perf(c, m)
  }

  def merge(p1: Performance, p2: Performance): Performance = (p1, p2) match {
    case (Nil, es2) => es2
    case (es1, Nil) => es1
    case (a@(e1 :: es1), b@(e2 :: es2)) =>
      if (e1.eTime < e2.eTime) e1 :: merge(es1, b)
      else e2 :: merge(a, es2)
  }

}

object Metronome {
  /**
   * Calculate the ticked duration from a duration
   *
   * @param tempoInBPM is the tempo in BPM
   * @param dur is the duration to convert
   * @return the ticked duration
   */
  def metro(tempoInBPM: Int, dur: Duration): DurT = {
//    val seconds = 60 / (tempoInBPM * dur)
    val ticksPerBeat = 96
//    val ticksPerSecond = ticksPerBeat * (tempoInBPM / 60.0)
//    ticksPerSecond * seconds
    ticksPerBeat / dur  // TODO
  }
}