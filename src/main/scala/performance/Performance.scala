package performance

import music.{:+:, :=:, Custom, Instrument, KeySig, Modify, Music, Note, Phrase, PhraseAttribute, Prim, Rest, Tempo, Transpose}
import music.Types.Duration
import performance.Performance.DurT
import performance.players.Player
import spire.math.Rational

object Performance {

  type Performance = List[MEvent]
  type PTime = Rational
  type DurT = Rational
  type PlayerName = String
  type PMap[A] = PlayerName => Player[A]
  type NoteFun[A] = Context[A] => Duration => A => Performance
  type PhraseFun[A] = PMap[A] => Context[A] => List[PhraseAttribute] => Music[A] => (Performance, DurT)

  def perform(): Performance = ??? // TODO default perform

  def hsomPerform[A](pm: PMap[A], c: Context[A], m: Music[A]): Performance =
    perf(pm, c, m)._1

  def perf[A](pm: PMap[A], c: Context[A], m: Music[A]): (Performance, DurT) = m match {
    case Prim(Note(d, p)) => (c.cPlayer.playNote(c)(d)(p), d * c.cDur)
    case Prim(Rest(d)) => (List.empty, d * c.cDur)
    case :+:(m1, m2) => {
      val (pf1, d1) = perf(pm, c, m1)
      val (pf2, d2) = perf(pm, c.copy(cTime = c.cTime + d1), m2)
      (pf1 ++ pf2, d1 + d2)
    }
    case :=:(m1, m2) => {
      val (pf1, d1) = perf(pm, c, m1)
      val (pf2, d2) = perf(pm, c, m2)
      (merge(pf1, pf2), d1 max d2)
    }
    case Modify(Tempo(r), m) => perf(pm, c.copy(cDur = c.cDur / r), m)
    case Modify(Transpose(p), m) => perf(pm, c.copy(cPch = c.cPch + p), m)
    case Modify(Instrument(i), m) => perf(pm, c.copy(cInst = i), m)
    case Modify(KeySig(pc, mo), m) => perf(pm, c.copy(cKey = (pc, mo)), m)
    case Modify(Phrase(pas), m) => c.cPlayer.interpPhrase(pm)(c)(pas)(m)
    case Modify(Custom(s), m) =>
      if (s.take(7) == "Player ") perf(pm, c.copy(cPlayer = pm(s.drop(7))), m)
      else perf(pm, c, m)
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
  def metro(settings: Int, dur: Duration): DurT =
    60 / (Rational(settings) * dur)
}