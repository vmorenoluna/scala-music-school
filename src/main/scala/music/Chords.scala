package music

import music.Types.{Duration, Octave, Pitch}
import Music._

object Chords {

  def dMinor(): Music[Pitch] = d(4,wn) :=: f(4,wn) :=: a(4,wn)
  def gMajor(): Music[Pitch] = g(4,wn) :=: b(4,wn) :=: d(4,wn)
  def cMajor(): Music[Pitch] = c(4,wn) :=: e(4,wn) :=: g(4,wn)

  def minor(root: Music[Pitch]): Music[Pitch] = {
    val minorThird = transpose(3, root)
    val perfectFifth = transpose(7, root)
    root :=: minorThird :=: perfectFifth
  }

  def major(root: Music[Pitch]): Music[Pitch] = {
    val majorThird = transpose(4, root)
    val perfectFifth = transpose(7, root)
    root :=: majorThird :=: perfectFifth
  }

  def twoFiveOne(p: Pitch, d: Duration): Music[Pitch] = {
    val ii = minor(transpose(3, note(d, p)))
    val V = major(transpose(7, note(d, p)))
    val I = major(transpose(12, note(2*d, p)))
    ii :+: V :+: I
  }

}
