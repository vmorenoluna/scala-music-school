import midi.MidiService
import music.Music._
import music.Music1._
import music.Music1.{Music1, Note1}
import music.Types.Pitch
import music.{AcousticGrandPiano, C, Major, Music}
import performance.Performance.hsomPerform
import performance.players.DefPlayer
import performance.{Context, Metronome}

object Main extends App {

//  val ev1 = MidiService.createNoteOnEvent(0, 60, 93, 0).get
//  val ev2 = MidiService.createNoteOffEvent(0, 60, 93, 64).get
//  val ev3 = MidiService.createNoteOnEvent(0, 60, 93, 128).get
//
//  val sequence: Sequence = new Sequence(Sequence.PPQ, resolution)
//  val track: Track = sequence.createTrack()
//  track.add(ev1)
//  track.add(ev2)
//  track.add(ev3)
//
//  val fileTest = new File("test0.midi")
//  MidiSystem.write(sequence, 0, fileTest)

//  val sequencer: Sequencer = MidiSystem.getSequencer()
//  sequencer.open()
//
//  sequencer.addMetaEventListener((metaMsg: MetaMessage) => {
//    if (metaMsg.getType() == 0x2F) {
//      sequencer.close();
//    }
//  })
//
//  //  play(sequencer, sequence)
//
//  sequencer.close()
//
//  def play(sequencer: Sequencer, sequence: Sequence): Unit = {
//    sequencer.setSequence(sequence)
//    sequencer.stop()
//    sequencer.setTickPosition(0)
//    sequencer.start();
//  }

  // val pitchArray = List(C4,C4,C4,D4,E4,E4,D4,E4,F4,G4,
  //    C5,C5,C5,G4,G4,G4,E4,E4,E4,
  //    C4,C4,C4,G4,F4,E4,D4,C4)
  //  val rhythmArray = List(C,C,CT,QT,C,CT,QT,CT, QT,
  //    M, QT, QT, QT, QT, QT,QT, QT, QT, QT, QT,
  //    QT, QT, CT, QT, CT, QT,M)
  //
  //  val notes = for {
  //    p <- pitchArray
  //    d <- rhythmArray
  //  } yield new Note(p, d)

  val music: Music[Pitch] =
    (c(5)(qn) :+: d(5)(en) :+: e(5)(sn)) :=: (c(6)(sn) :+: d(6)(qn))

  val music1: Music1 = music.toMusic1()
  val context: Context[Note1] = Context(0, DefPlayer, AcousticGrandPiano, Metronome.metro(120, qn), 0, 127, (C, Major))
  val performance = hsomPerform(context, music1)

  MidiService.writePerformance(performance, "test.mid")

}
