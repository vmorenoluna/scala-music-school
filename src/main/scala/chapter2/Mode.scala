package chapter2

sealed trait Mode

final case object Major extends Mode
final case object Minor extends Mode
final case object Ionian extends Mode
final case object Dorian extends Mode
final case object Phrygian extends Mode
final case object Lydian extends Mode
final case object Mixolydian extends Mode
final case object Aeolian extends Mode
final case object Locrian extends Mode
final case class CustomMode(name: String) extends Mode
