package music

import music.Types._
import spire.math.Rational
import spire.math.Rational.zero

import scala.collection.immutable.LazyList.#::
import scala.math.{abs, max, min}

sealed trait Primitive[A]

final case class Note[A](duration: Duration, features: A) extends Primitive[A]

final case class Rest[A](duration: Duration) extends Primitive[A]

sealed trait Music[A] {
  def :+:(that: Music[A]): Music[A] = new :+:(that, this)

  def :=:(that: Music[A]): Music[A] = new :=:(that, this)

  def /=:(that: Music[A]): Music[A] = Music.cutL(Music.durL(that), this) :=: Music.cutL(Music.durL(this), that)
}

final case class Prim[A](primitive: Primitive[A]) extends Music[A]

final case class Modify[A](control: Control, music: Music[A]) extends Music[A]

final case class :+:[A](m: Music[A], n: Music[A]) extends Music[A] // TODO sequential composition
final case class :=:[A](m: Music[A], n: Music[A]) extends Music[A] // TODO parallel composition

final object Music {

  def note[A](d: Duration, features: A): Music[A] = Prim(Note(d, features))

  def rest[A](d: Duration): Music[A] = Prim(Rest(d))

  def tempo[A](d: Duration, m: Music[A]): Music[A] = Modify(Tempo(d), m)

  def transpose[A](i: AbsPitch, m: Music[A]): Music[A] = Modify(Transpose(i), m)

  def instrument[A](i: InstrumentName, m: Music[A]): Music[A] = Modify(Instrument(i), m)

  def phrase[A](pa: List[PhraseAttribute], m: Music[A]): Music[A] = Modify(Phrase(pa), m)

  def keysig[A](pc: PitchClass, mo: Mode, m: Music[A]): Music[A] = Modify(KeySig(pc, mo), m)

  def cff(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Cff, o))

  def cf(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Cf, o))

  def c(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (C, o))

  def cs(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Cs, o))

  def css(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Css, o))

  def dff(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Dff, o))

  def df(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Df, o))

  def d(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (D, o))

  def ds(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Ds, o))

  def dss(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Dss, o))

  def eff(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Eff, o))

  def ef(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Ef, o))

  def e(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (E, o))

  def es(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Es, o))

  def ess(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Ess, o))

  def fff(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Fff, o))

  def ff(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Ff, o))

  def f(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (F, o))

  def fs(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Fs, o))

  def fss(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Fss, o))

  def gff(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Gff, o))

  def gf(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Gf, o))

  def g(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (G, o))

  def gs(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Gs, o))

  def gss(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Gss, o))

  def aff(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Aff, o))

  def af(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Af, o))

  def a(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (A, o))

  def as(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (As, o))

  def ass(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Ass, o))

  def bff(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Bff, o))

  def bf(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Bf, o))

  def b(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (B, o))

  def bs(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Bs, o))

  def bss(o: Octave): Duration => Music[Pitch] = (d: Duration) => note(d, (Bss, o))

  def bn: Duration = 2

  def wn: Duration = 1

  def hn: Duration = 1 div 2

  def qn: Duration = 1 div 4

  def en: Duration = 1 div 8

  def sn: Duration = 1 div 16

  def tn: Duration = 1 div 32

  def sfn: Duration = 1 div 64

  def dwn: Duration = 3 div 2

  def dhn: Duration = 3 div 4

  def dqn: Duration = 3 div 8

  def den: Duration = 3 div 16

  def dsn: Duration = 3 div 32

  def dtn: Duration = 3 div 64

  def ddhn: Duration = 7 div 8

  def ddqn: Duration = 7 div 16

  def dden: Duration = 7 div 32

  def bnr: Music[Pitch] = rest(bn)

  def wnr: Music[Pitch] = rest(wn)

  def hnr: Music[Pitch] = rest(hn)

  def qnr: Music[Pitch] = rest(qn)

  def enr: Music[Pitch] = rest(en)

  def snr: Music[Pitch] = rest(sn)

  def tnr: Music[Pitch] = rest(tn)

  def sfnr: Music[Pitch] = rest(sfn)

  def dwnr: Music[Pitch] = rest(dwn)

  def dhnr: Music[Pitch] = rest(dhn)

  def dqnr: Music[Pitch] = rest(dqn)

  def denr: Music[Pitch] = rest(den)

  def dsnr: Music[Pitch] = rest(dsn)

  def dtnr: Music[Pitch] = rest(dtn)

  def ddhnr: Music[Pitch] = rest(ddhn)

  def ddqnr: Music[Pitch] = rest(ddqn)

  def ddenr: Music[Pitch] = rest(dden)

  def line[A](notes: List[Music[A]]): Music[A] = notes.foldRight(rest[A](0))(_ :+: _)

  def chord[A](notes: List[Music[A]]): Music[A] = notes.foldRight(rest[A](0))(_ :=: _)

  def maxPitch(pitches: List[Pitch]): Pitch = pitches.foldRight(pitch(0))(!!!)

  def maxAbsPitch(absPitches: List[AbsPitch]): AbsPitch = absPitches.foldLeft(Int.MinValue)(max)

  def minAbsPitch(absPitches: List[AbsPitch]): AbsPitch = absPitches.foldLeft(Int.MaxValue)(min)

  def fuse[A](ds: List[Duration], notes: List[Duration => Music[A]]): List[Music[A]] =
    ds zip notes map { case (a, b) => b(a) }

  def times[A](n: Int, m: Music[A]): Music[A] = n match {
    case 0 => rest(0)
    case n => m :+: times(n - 1, m)
  }

  def addDuration[A](d: Duration, notes: List[Duration => Music[A]]): Music[A] =
    line(notes.map(note => note(d)))

  // TODO force a single note in the signature
  def graceNote(step: Step, m: Music[Pitch]): Music[Pitch] = m match {
    case Prim(Note(d, p: Pitch)) => note(d / 8, trans(step, p)) :+: note(7 * d / 8, p)
    case _ => m
  }

  def grace(step: Step, fraction: Rational, m: Music[Pitch]): Music[Pitch] = (step, fraction, m) match {
    case (n, r, Prim(Note(d, p: Pitch))) => note(r * d, trans(n, p)) :+: note((1 - r) * d, p)
    case _ => m
  }

  def grace2(step: Step, fraction: Rational, m1: Music[Pitch], m2: Music[Pitch]): Music[Pitch] = (step, fraction, m1, m2) match {
    case (n, r, Prim(Note(d1, p1: Pitch)), Prim(Note(d2, p2: Pitch))) =>
      note(d1 - r * d2, p1) :+: note(r * d2, trans(n, p2)) :+: note(d2, p2)
    case _ => m2
  }

  def apPairs(aps1: List[AbsPitch], aps2: List[AbsPitch]): List[(AbsPitch, AbsPitch)] =
    for {
      ap1 <- aps1
      ap2 <- aps2
      diff = abs(ap1 - ap2)
      if (diff > 2 && diff < 8)
    } yield (ap1, ap2)

  def apPairsMusic(pairs: List[(AbsPitch, AbsPitch)]): Music[Pitch] =
    line(
      for {
        pair <- pairs
        duration = if (pair._1 % 2 == 0) qn else en
        chord = note(duration, pitch(pair._1)) :=: note(duration, pitch(pair._2))
      } yield chord
    )

  def offset[A](d: Duration, m: Music[A]): Music[A] =
    rest[A](d) :+: m

  def forever[A](m: Music[A]): Music[A] =
    m :+: forever(m)

  def lineToList[A](m: Music[A]): List[Music[A]] = m match {
    case Prim(Rest(d)) if d == 0 => List.empty
    case Prim(Note(d, f)) => List(Prim(Note(d, f)))
    case :+:(n, ns) => n :: lineToList(ns)
    case _ => List.empty // TODO music not created with line, return error
  }

  def invert(m: Music[Pitch]): Music[Pitch] = {
    val l@(Prim(Note(_, r)) :: _) = lineToList(m)
    line(
      l.map {
        case Prim(Note(d, p)) => note(d, pitch(2 * absPitch(r) - absPitch(p)))
        case Prim(Rest(d)) => rest(d)
        case _ => rest(0) // TODO
      }
    )
  }

  //  def retro(m: Music[Pitch]): Music[Pitch] =
  //    line(lineToList(m).reverse)

  def retro[A](m: Music[A]): Music[A] = m match {
    case n: Prim[A] => n
    case Modify(control, music) => Modify(control, retro(music))
    case :+:(m1, m2) => retro(m2) :+: retro(m1)
    case :=:(m1, m2) => {
      val d1 = dur(m1)
      val d2 = dur(m2)
      if (d1 > d2) retro(m1) :=: (rest[A](d1 - d2) :+: retro(m2))
      else (rest[A](d2 - d1) :+: retro(m1)) :=: retro(m2)
    }
  }

  def retroInvert(m: Music[Pitch]): Music[Pitch] =
    retro(invert(m))

  def invertRetro(m: Music[Pitch]): Music[Pitch] =
    invert(retro(m))

  def properRow(m: Music[Pitch]): Boolean =
    pitches(m, p => pitch(absPitch(p)))
      .groupBy(_._1)
      .keys
      .size == 12

  private def pitches(m: Music[Pitch], f: Pitch => Pitch): List[Pitch] = m match {
    case Prim(Rest(_)) => List.empty
    case Prim(Note(_, p)) => List(f(p))
    case :+:(Prim(Rest(_)), ns) => pitches(ns, f)
    case :+:(Prim(Note(_, p)), ns) => f(p) :: pitches(ns, f)
    case _ => List.empty // TODO music not created with line, return error
  }

  def palin(m: Music[Pitch]): Boolean = {
    val ps = pitches(m, identity)
    ps equals ps.reverse
  }

  def retroPitches(m: Music[(PitchClass, Octave)]): Music[Pitch] = {
    val music = lineToList(m)
    val musicZipped: List[(Music[Pitch], Music[Pitch])] = music.zip(music.reverse)

    def go(list: List[(Music[Pitch], Music[Pitch])]): Music[Pitch] =
      list match {
        case Nil => rest(0)
        case ::((Prim(Note(d, _)), Prim(Note(_, p))), tail) => note(d, p) :+: go(tail)
        case _ => rest(0) // TODO
      }

    go(musicZipped)
  }

  def dur[A](m: Music[A]): Duration = m match {
    case Prim(Note(d, _)) => d
    case Prim(Rest(d)) => d
    case Modify(Tempo(r), m) => dur(m) / r
    case Modify(_, m) => dur(m)
    case :+:(m1, m2) => dur(m1) + dur(m2)
    case :=:(m1, m2) => dur(m1) max dur(m2)
  }

  private def mergeLD(ld1: LazyDur, ld2: LazyDur): LazyDur = (ld1, ld2) match {
    case (LazyNil, ld) => ld
    case (ld, LazyNil) => ld
    case (d1 #:: ds1, d2 #:: ds2) => if (d1 < d2) d1 +: mergeLD(ds1, ld2) else d2 +: mergeLD(ld1, ds2)
  }

  def durL[A](m: Music[A]): LazyDur = m match {
    case Prim(_) => LazyList(dur(m))
    case Modify(Tempo(r), m) => durL(m).map(_ / r)
    case Modify(_, m) => durL(m)
    case :+:(m1, m2) => {
      val d1 = durL(m1)
      d1 ++ durL(m2).map(_ + d1.last)
    }
    case :=:(m1, m2) => mergeLD(durL(m1), durL(m2))
  }

  def cut[A](d: Duration, m: Music[A]): Music[A] = m match {
    case _ if d <= 0 => rest(0)
    case Prim(Note(oldD, p)) => note(oldD min d, p)
    case Prim(Rest(oldD)) => rest(oldD min d)
    case Modify(Tempo(r), m) => tempo(r, cut(d * r, m))
    case Modify(c, m) => Modify(c, cut(d, m))
    case :+:(m1, m2) => {
      val m3 = cut[A](d, m1)
      val m4 = cut[A](d - dur(m3), m2)
      m3 :+: m4
    }
    case :=:(m1, m2) => cut(d, m1) :=: cut(d, m2)
  }

  private def minL(ld: LazyDur, d: Duration): Duration = (ld, d) match {
    case (LazyNil, _) => zero
    case (LazyList(d1), d2) => d1 min d2
    case (d1 #:: ds, d2) => if (d1 < d2) minL(ds, d2) else d2
  }

  def cutL[A](ld: LazyDur, m: Music[A]): Music[A] = (ld, m) match {
    case (LazyNil, _) => rest(zero)
    case (d #:: ds, m) if (d <= zero) => cutL(ds, m)
    case (ld, Prim(Note(oldD, p))) => note(minL(ld, oldD), p)
    case (ld, Prim(Rest(oldD))) => rest(minL(ld, oldD))
    case (ld, Modify(Tempo(r), m)) => tempo(r, cutL(ld.map(_ * r), m))
    case (ld, Modify(c, m)) => Modify(c, cutL(ld, m))
    case (ld, :+:(m1, m2)) => {
      val m3 = cutL[A](ld, m1)
      val m4 = cutL[A](ld.map(d => d - dur(m3)), m2)
      m3 :+: m4
    }
    case (ld, :=:(m1, m2)) => cutL(ld, m1) :=: cutL(ld, m2)
  }

  def remove[A](d: Duration, m: Music[A]): Music[A] = m match {
    case _ if d <= 0 => m
    case Prim(Note(oldD, p)) => note((oldD - d) max 0, p)
    case Prim(Rest(oldD)) => rest((oldD - d) max 0)
    case Modify(Tempo(r), m) => tempo(r, remove(d * r, m))
    case Modify(c, m) => Modify(c, remove(d, m))
    case :+:(m1, m2) => {
      val m3 = remove[A](d, m1)
      val m4 = remove[A](d - dur(m1), m2)
      m3 :+: m4
    }
    case :=:(m1, m2) => remove(d, m1) :=: remove(d, m2)
  }

  def removeZeros[A](m: Music[A]): Music[A] = m match {
    case Prim(p) => Prim(p)
    case Modify(c, m) => Modify(c, removeZeros(m))
    case :=:(m1, m2) => (removeZeros(m1), removeZeros(m2)) match {
      case (Prim(Note(d, _)), m) if (d == zero) => m
      case (Prim(Rest(d)), m) if (d == zero) => m
      case (m, Prim(Note(d, _))) if (d == zero) => m
      case (m, Prim(Rest(d))) if (d == zero) => m
      case (m1, m2) => m1 :=: m2
    }
    case :+:(m1, m2) => (removeZeros(m1), removeZeros(m2)) match {
      case (Prim(Note(d, _)), m) if (d == zero) => m
      case (Prim(Rest(d)), m) if (d == zero) => m
      case (m, Prim(Note(d, _))) if (d == zero) => m
      case (m, Prim(Rest(d))) if (d == zero) => m
      case (m1, m2) => m1 :+: m2
    }
  }

  def trill(interval: Int, d: Duration, m: Music[Pitch]): Music[Pitch] = (interval, d, m) match {
    case (i, sDur, Prim(Note(tDur, p))) =>
      if (sDur >= tDur)
        note(tDur, p)
      else
        note(sDur, p) :+: trill(-i, sDur, note(tDur - sDur, trans(i, p)))
    case (i, d, Modify(Tempo(r), m)) => tempo(r, trill(i, d * r, m))
    case (i, d, Modify(c, m)) => Modify(c, trill(i, d, m))
    case _ => rest(zero) // TODO input must be a single note
  }

  def trillOtherNote(interval: Int, d: Duration, m: Music[Pitch]): Music[Pitch] =
    trill(-interval, d, transpose(interval, m))

  def trilln(interval: Int, nTimes: Int, m: Music[Pitch]): Music[Pitch] =
    trill(interval, dur(m) / Rational(nTimes), m)

  def trillnOtherNote(interval: Int, nTimes: Int, m: Music[Pitch]): Music[Pitch] =
    trilln(-interval, nTimes, transpose(interval, m))

  def roll(d: Duration, m: Music[Pitch]): Music[Pitch] =
    trill(0, d, m)

  def rolln(nTimes: Int, m: Music[Pitch]): Music[Pitch] =
    trilln(0, nTimes, m)

  def perc(ps: PercussionSound.Value, d: Duration): Music[Pitch] =
    instrument(Percussion, note(d, pitch(ps.id + 35)))

}