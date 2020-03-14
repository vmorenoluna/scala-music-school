package music

import music.Types.{AbsPitch, PhraseAttribute, Tempo => TempoValue}

sealed trait Control

final case class Tempo(value: TempoValue) extends Control
final case class Transpose(value: AbsPitch) extends Control
final case class Instrument(instrumentName: InstrumentName) extends Control
final case class Phrase(attributes: List[PhraseAttribute]) extends Control
final case class KeySig(pitchClass: PitchClass, mode: Mode) extends Control
final case class Custom(value: String) extends Control
