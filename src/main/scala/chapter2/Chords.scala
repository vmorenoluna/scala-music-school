package chapter2

import chapter2.Types.Pitch
import Music._

object Chords {

  def dMinor(): Music[Pitch] = d(4,wn) :=: f(4,wn) :=: a(4,wn)
  def gMajor(): Music[Pitch] = g(4,wn) :=: b(4,wn) :=: d(4,wn)
  def cMajor(): Music[Pitch] = c(4,wn) :=: e(4,wn) :=: g(4,wn)
}
