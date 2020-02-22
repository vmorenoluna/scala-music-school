package music

import music.Types.{Duration, Octave, Pitch}
import Music._

sealed trait BluesPitchClass
final case object Root extends BluesPitchClass
final case object MinorThird extends BluesPitchClass
final case object Fourth extends BluesPitchClass
final case object Fifth extends BluesPitchClass
final case object MinorSeventh extends BluesPitchClass

class Blues {

  type BluesPitch = (BluesPitchClass, Octave)

  def ro(o: Octave, d: Duration): Music[BluesPitch] = note(d, (Root, o))
  def mt(o: Octave, d: Duration): Music[BluesPitch] = note(d, (MinorThird, o))
  def fo(o: Octave, d: Duration): Music[BluesPitch] = note(d, (Fourth, o))
  def fi(o: Octave, d: Duration): Music[BluesPitch] = note(d, (Fifth, o))
  def ms(o: Octave, d: Duration): Music[BluesPitch] = note(d, (MinorSeventh, o))

  def fromBlues(m: Music[BluesPitch]): Music[Pitch] = m match {
    case Prim(Note(d, (Root, o))) => Prim(Note(d, (C, o)))
    case Prim(Note(d, (MinorThird, o))) => Prim(Note(d, (Ef, o)))
    case Prim(Note(d, (Fourth, o))) => Prim(Note(d, (F, o)))
    case Prim(Note(d, (Fifth, o))) => Prim(Note(d, (G, o)))
    case Prim(Note(d, (MinorSeventh, o))) => Prim(Note(d, (Bf, o)))
    case Prim(Rest(d)) => Prim(Rest(d))
    case Modify(control, music) => Modify(control, fromBlues(music))
    case :+:(m, n) => fromBlues(m) :+: fromBlues(n)
    case :=:(m, n) => fromBlues(m) :=: fromBlues(n)
  }

}
