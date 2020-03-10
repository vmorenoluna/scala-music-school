package music

sealed trait Modes

final case object Major extends Modes
final case object Minor extends Modes
final case object Ionian extends Modes
final case object Dorian extends Modes
final case object Phrygian extends Modes
final case object Lydian extends Modes
final case object Mixolydian extends Modes
final case object Aeolian extends Modes
final case object Locrian extends Modes
final case class CustomMode(name: String) extends Modes
