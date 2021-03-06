package music

sealed trait InstrumentName

final case object AcousticGrandPiano extends InstrumentName

final case object BrightAcousticPiano extends InstrumentName

final case object ElectricGrandPiano extends InstrumentName

final case object HonkyTonkPiano extends InstrumentName

final case object RhodesPiano extends InstrumentName

final case object ChorusedPiano extends InstrumentName

final case object Harpsichord extends InstrumentName

final case object Clavinet extends InstrumentName

final case object Celesta extends InstrumentName

final case object Glockenspiel extends InstrumentName

final case object MusicBox extends InstrumentName

final case object Vibraphone extends InstrumentName

final case object Marimba extends InstrumentName

final case object Xylophone extends InstrumentName

final case object TubularBells extends InstrumentName

final case object Dulcimer extends InstrumentName

final case object HammondOrgan extends InstrumentName

final case object PercussiveOrgan extends InstrumentName

final case object RockOrgan extends InstrumentName

final case object ChurchOrgan extends InstrumentName

final case object ReedOrgan extends InstrumentName

final case object Accordion extends InstrumentName

final case object Harmonica extends InstrumentName

final case object TangoAccordion extends InstrumentName

final case object AcousticGuitarNylon extends InstrumentName

final case object AcousticGuitarSteel extends InstrumentName

final case object ElectricGuitarJazz extends InstrumentName

final case object ElectricGuitarClean extends InstrumentName

final case object ElectricGuitarMuted extends InstrumentName

final case object OverdrivenGuitar extends InstrumentName

final case object DistortionGuitar extends InstrumentName

final case object Guitarharmonics extends InstrumentName

final case object AcousticBass extends InstrumentName

final case object ElectricBassFingered extends InstrumentName

final case object ElectricBassPicked extends InstrumentName

final case object FretlessBass extends InstrumentName

final case object SlapBass1 extends InstrumentName

final case object SlapBass2 extends InstrumentName

final case object SynthBass1 extends InstrumentName

final case object SynthBass2 extends InstrumentName

final case object Violin extends InstrumentName

final case object Viola extends InstrumentName

final case object Cello extends InstrumentName

final case object Contrabass extends InstrumentName

final case object TremoloStrings extends InstrumentName

final case object PizzicatoStrings extends InstrumentName

final case object OrchestralHarp extends InstrumentName

final case object Timpani extends InstrumentName

final case object StringEnsemble1 extends InstrumentName

final case object StringEnsemble2 extends InstrumentName

final case object SynthStrings1 extends InstrumentName

final case object SynthStrings2 extends InstrumentName

final case object ChoirAahs extends InstrumentName

final case object VoiceOohs extends InstrumentName

final case object SynthVoice extends InstrumentName

final case object OrchestraHit extends InstrumentName

final case object Trumpet extends InstrumentName

final case object Trombone extends InstrumentName

final case object Tuba extends InstrumentName

final case object MutedTrumpet extends InstrumentName

final case object FrenchHorn extends InstrumentName

final case object BrassSection extends InstrumentName

final case object SynthBrass1 extends InstrumentName

final case object SynthBrass2 extends InstrumentName

final case object SopranoSax extends InstrumentName

final case object AltoSax extends InstrumentName

final case object TenorSax extends InstrumentName

final case object BaritoneSax extends InstrumentName

final case object Oboe extends InstrumentName

final case object Bassoon extends InstrumentName

final case object EnglishHorn extends InstrumentName

final case object Clarinet extends InstrumentName

final case object Piccolo extends InstrumentName

final case object Flute extends InstrumentName

final case object Recorder extends InstrumentName

final case object PanFlute extends InstrumentName

final case object BlownBottle extends InstrumentName

final case object Shakuhachi extends InstrumentName

final case object Whistle extends InstrumentName

final case object Ocarina extends InstrumentName

final case object Lead1Square extends InstrumentName

final case object Lead2Sawtooth extends InstrumentName

final case object Lead3Calliope extends InstrumentName

final case object Lead4Chiff extends InstrumentName

final case object Lead5Charang extends InstrumentName

final case object Lead6Voice extends InstrumentName

final case object Lead7Fifths extends InstrumentName

final case object Lead8BassLead extends InstrumentName

final case object Pad1Newage extends InstrumentName

final case object Pad2Warm extends InstrumentName

final case object Pad3Polysynth extends InstrumentName

final case object Pad4Choir extends InstrumentName

final case object Pad5Bowed extends InstrumentName

final case object Pad6Metallic extends InstrumentName

final case object Pad7Halo extends InstrumentName

final case object Pad8Sweep extends InstrumentName

final case object FX1Train extends InstrumentName

final case object FX2Soundtrack extends InstrumentName

final case object FX3Crystal extends InstrumentName

final case object FX4Atmosphere extends InstrumentName

final case object FX5Brightness extends InstrumentName

final case object FX6Goblins extends InstrumentName

final case object FX7Echoes extends InstrumentName

final case object FX8SciFi extends InstrumentName

final case object Sitar extends InstrumentName

final case object Banjo extends InstrumentName

final case object Shamisen extends InstrumentName

final case object Koto extends InstrumentName

final case object Kalimba extends InstrumentName

final case object Bagpipe extends InstrumentName

final case object Fiddle extends InstrumentName

final case object Shanai extends InstrumentName

final case object TinkleBell extends InstrumentName

final case object Agogo extends InstrumentName

final case object SteelDrums extends InstrumentName

final case object Woodblock extends InstrumentName

final case object TaikoDrum extends InstrumentName

final case object MelodicDrum extends InstrumentName

final case object SynthDrum extends InstrumentName

final case object ReverseCymbal extends InstrumentName

final case object GuitarFretNoise extends InstrumentName

final case object BreathNoise extends InstrumentName

final case object Seashore extends InstrumentName

final case object BirdTweet extends InstrumentName

final case object TelephoneRing extends InstrumentName

final case object Helicopter extends InstrumentName

final case object Applause extends InstrumentName

final case object Gunshot extends InstrumentName

final case object Percussion extends InstrumentName

final case class CustomInstrument(name: String) extends InstrumentName

final object PercussionSound extends Enumeration {
  val AcousticBassDrum = Value(0)  // MIDI Key 35
  val BassDrum1 = Value(1)         // MIDI Key 36
  val SideStick = Value(2)
  val AcousticSnare = Value(3)
  val HandClap = Value(4)
  val ElectricSnare = Value(5)
  val LowFloorTom = Value(6)
  val ClosedHiHat = Value(7)
  val HighFloorTom = Value(8)
  val PedalHiHat = Value(9)
  val LowTom = Value(10)
  val OpenHiHat = Value(11)
  val LowMidTom = Value(12)
  val HiMidTom = Value(13)
  val CrashCymbal1 = Value(14)
  val HighTom = Value(15)
  val RideCymbal1 = Value(16)
  val ChineseCymbal = Value(17)
  val RideBell = Value(18)
  val Tambourine = Value(19)
  val SplashCymbal = Value(20)
  val Cowbell = Value(21)
  val CrashCymbal2 = Value(22)
  val Vibraslap = Value(23)
  val RideCymbal2 = Value(24)
  val HiBongo = Value(25)
  val LowBongo = Value(26)
  val MuteHiConga = Value(27)
  val OpenHiConga = Value(28)
  val LowConga = Value(29)
  val HighTimbale = Value(30)
  val LowTimbale = Value(31)
  val HighAgogo = Value(32)
  val LowAgogo = Value(33)
  val Cabasa = Value(34)
  val Maracas = Value(35)
  val ShortWhistle = Value(36)
  val LongWhistle = Value(37)
  val ShortGuiro = Value(38)
  val LongGuiro = Value(39)
  val Claves = Value(40)
  val HiWoodBlock = Value(41)
  val LowWoodBlock = Value(42)
  val MuteCuica = Value(43)
  val OpenCuica = Value(44)
  val MuteTriangle = Value(45)
  val OpenTriangle = Value(46)    // MIDI Key 82
}