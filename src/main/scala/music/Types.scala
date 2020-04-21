package music

import cats.kernel.Eq
import spire.math.Rational

import scala.math.Integral.Implicits._

final object Types {
  type Octave = Int
  type Pitch = (PitchClass, Octave)
  type Duration = Rational
  // non decreasing list of durations such that the last element in the list is the actual duration,
  // and an infinite list implies an infinite duration
  type LazyDur = LazyList[Duration]
  val LazyNil = LazyList.empty
  type AbsPitch = Int
  type Tempo = Rational
  type PhraseAttribute = Any     // TODO
  type Step = Int
  type Volume = Int

  final implicit class Fraction(private val self: Int) extends AnyVal {
    def div(other: Int) = Rational(self, other)
  }

  implicit val octaveEq: Eq[Octave] = Eq.fromUniversalEquals
  implicit val pitchEq: Eq[Pitch] = Eq.fromUniversalEquals

  def absPitch(p: Pitch): AbsPitch = 12 * (p._2) + pcToInt(p._1)

  def pcToInt(pitchClass: PitchClass): Int = pitchClass match {
    case Cff => -2
    case Cf => -1
    case C => 0
    case Dff => 0
    case Cs => 1
    case Df => 1
    case Css => 2
    case D => 2
    case Eff => 2
    case Ds => 3
    case Ef => 3
    case Fff => 3
    case Dss => 4
    case E => 4
    case Ff => 4
    case Es => 5
    case F => 5
    case Gff => 5
    case Ess => 6
    case Fs => 6
    case Gf => 6
    case Fss => 7
    case G => 7
    case Aff => 7
    case Gs => 8
    case Af => 8
    case Gss => 9
    case A => 9
    case Bff => 9
    case As => 10
    case Bf => 10
    case Ass => 11
    case B => 11
    case Bs => 12
    case Bss => 13
  }

  def pitch(ap: AbsPitch): Pitch = {
    val (oct, n) = ap /% 12
    val pitchClass = List(C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B)(n)
    (pitchClass, oct)
  }

  def trans(value: Int, p: Pitch): Pitch = pitch(absPitch(p) + value)

  def transM(ap: AbsPitch, mp: Music[Pitch]): Music[Pitch] = mp match {
    case Prim(Note(d, p)) => Prim(Note(d, trans(ap, p)))
    case Prim(Rest(d)) => Prim(Rest(d))
    case :+:(m, n) => transM(ap, m) :+: transM (ap, n)
    case :=:(m, n) => transM(ap, m) :=: transM (ap, n)
    case Modify(control, music) => Modify(control, transM(ap, mp))
  }

  def toAbsPitches(pitches: List[Pitch]): List[AbsPitch] = pitches.map(absPitch(_))
  def toPitches(absPitches: List[AbsPitch]): List[Pitch] = absPitches.map(pitch(_))
  def !!!(p1: Pitch, p2: Pitch): Pitch = if (absPitch(p1) > absPitch(p2)) p1 else p2

  def hs(): Step = 1
  def ws(): Step = 2

}
