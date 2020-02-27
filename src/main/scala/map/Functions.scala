package map

import music.{:+:, :=:, Modify, Music, Note, PitchClass, Prim, Rest}
import music.Types._
import music.Music._

object Functions {

  def f1(n: Int, ps: List[Pitch]): List[(PitchClass, Octave)] = ps.map(trans(n, _))

  def f2(ds: List[Duration]): List[Music[Nothing]] = ds.map(rest)

  def f3(mps: List[Music[Pitch]]): List[Music[Pitch]] = mps match {
    case Nil => Nil
    case ::(head, tail) => staccato(head) :: f3(tail)
  }

  def staccato(note: Music[Pitch]): Music[Pitch] = note match {
    case Prim(Note(d, pitch)) => Prim(Note(d/2, pitch))
    case Prim(Rest(d)) => Prim(Rest(d))
    case Modify(control, m) => Modify(control, staccato(m))
    case m1 :+: m2 => staccato(m1) :+: staccato(m2)
    case m1 :=: m2 => staccato(m1) :=: staccato(m2)
  }

}
