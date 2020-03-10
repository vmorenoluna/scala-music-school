package music

import music.Types._
import scala.math.{max, min}

sealed trait Primitive[A]
final case class Note[A](duration: Duration, features: A) extends Primitive[A]
final case class Rest[A](duration: Duration) extends Primitive[A]

sealed trait Music[A] {
  def :+:(that: Music[A]): Music[A] = new :+:(this, that)
  def :=:(that: Music[A]): Music[A] = new :=:(this, that)
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
  def keysig[A](pc: PitchClass, mo: Modes, m: Music[A]): Music[A] = Modify(KeySig(pc, mo), m)

  def cff(o: Octave, d: Duration): Music[Pitch] = note(d, (Cff, o))
  def cf(o: Octave, d: Duration): Music[Pitch] = note(d, (Cf, o))
  def c(o: Octave, d: Duration): Music[Pitch] = note(d, (C, o))
  def cs(o: Octave, d: Duration): Music[Pitch] = note(d, (Cs, o))
  def css(o: Octave, d: Duration): Music[Pitch] = note(d, (Css, o))
  def dff(o: Octave, d: Duration): Music[Pitch] = note(d, (Dff, o))
  def df(o: Octave, d: Duration): Music[Pitch] = note(d, (Df, o))
  def d(o: Octave, d: Duration): Music[Pitch] = note(d, (D, o))
  def ds(o: Octave, d: Duration): Music[Pitch] = note(d, (Ds, o))
  def dss(o: Octave, d: Duration): Music[Pitch] = note(d, (Dss, o))
  def eff(o: Octave, d: Duration): Music[Pitch] = note(d, (Eff, o))
  def ef(o: Octave, d: Duration): Music[Pitch] = note(d, (Ef, o))
  def e(o: Octave, d: Duration): Music[Pitch] = note(d, (E, o))
  def es(o: Octave, d: Duration): Music[Pitch] = note(d, (Es, o))
  def ess(o: Octave, d: Duration): Music[Pitch] = note(d, (Ess, o))
  def fff(o: Octave, d: Duration): Music[Pitch] = note(d, (Fff, o))
  def ff(o: Octave, d: Duration): Music[Pitch] = note(d, (Ff, o))
  def f(o: Octave, d: Duration): Music[Pitch] = note(d, (F, o))
  def fs(o: Octave, d: Duration): Music[Pitch] = note(d, (Fs, o))
  def fss(o: Octave, d: Duration): Music[Pitch] = note(d, (Fss, o))
  def gff(o: Octave, d: Duration): Music[Pitch] = note(d, (Gff, o))
  def gf(o: Octave, d: Duration): Music[Pitch] = note(d, (Gf, o))
  def g(o: Octave, d: Duration): Music[Pitch] = note(d, (G, o))
  def gs(o: Octave, d: Duration): Music[Pitch] = note(d, (Gs, o))
  def gss(o: Octave, d: Duration): Music[Pitch] = note(d, (Gss, o))
  def aff(o: Octave, d: Duration): Music[Pitch] = note(d, (Aff, o))
  def af(o: Octave, d: Duration): Music[Pitch] = note(d, (Af, o))
  def a(o: Octave, d: Duration): Music[Pitch] = note(d, (A, o))
  def as(o: Octave, d: Duration): Music[Pitch] = note(d, (As, o))
  def ass(o: Octave, d: Duration): Music[Pitch] = note(d, (Ass, o))
  def bff(o: Octave, d: Duration): Music[Pitch] = note(d, (Bff, o))
  def bf(o: Octave, d: Duration): Music[Pitch] = note(d, (Bf, o))
  def b(o: Octave, d: Duration): Music[Pitch] = note(d, (B, o))
  def bs(o: Octave, d: Duration): Music[Pitch] = note(d, (Bs, o))
  def bss(o: Octave, d: Duration): Music[Pitch] = note(d, (Bss, o))

  def bn(): Duration = 2
  def wn(): Duration = 1
  def hn(): Duration = 1/2
  def qn(): Duration = 1/4
  def en(): Duration = 1/8
  def sn(): Duration = 1/16
  def tn(): Duration = 1/32
  def sfn(): Duration = 1/64
  def dwn(): Duration = 3/2
  def dhn(): Duration = 3/4
  def dqn(): Duration = 3/8
  def den(): Duration = 3/16
  def dsn(): Duration = 3/32
  def dtn(): Duration = 3/64
  def ddhn(): Duration = 7/8
  def ddqn(): Duration = 7/16
  def dden(): Duration = 7/32

  def bnr(): Music[Pitch] = rest(bn)
  def wnr(): Music[Pitch] = rest(wn)
  def hnr(): Music[Pitch] = rest(hn)
  def qnr(): Music[Pitch] = rest(qn)
  def enr(): Music[Pitch] = rest(en)
  def snr(): Music[Pitch] = rest(sn)
  def tnr(): Music[Pitch] = rest(tn)
  def sfnr(): Music[Pitch] = rest(sfn)
  def dwnr(): Music[Pitch] = rest(dwn)
  def dhnr(): Music[Pitch] = rest(dhn)
  def dqnr(): Music[Pitch] = rest(dqn)
  def denr(): Music[Pitch] = rest(den)
  def dsnr(): Music[Pitch] = rest(dsn)
  def dtnr(): Music[Pitch] = rest(dtn)
  def ddhnr(): Music[Pitch] = rest(ddhn)
  def ddqnr(): Music[Pitch] = rest(ddqn)
  def ddenr(): Music[Pitch] = rest(dden)

  def line[A](notes: List[Music[A]]): Music[A] = notes.fold(rest[A](0))(_:+:_)
  def chord[A](notes: List[Music[A]]): Music[A] = notes.fold(rest[A](0))(_:=:_)
  def maxPitch(pitches: List[Pitch]): Pitch = pitches.fold(pitch(0))(!!!)

  def maxAbsPitch(absPitches: List[AbsPitch]): AbsPitch = absPitches.foldLeft(0)(max)
  def minAbsPitch(absPitches: List[AbsPitch]): AbsPitch = absPitches.foldLeft(0)(min)

}