import jm.constants.Pitches._
import jm.constants.Durations._
import jm.constants.ProgramChanges._
import jm.music.data._
import jm.music.tools.Mod
import jm.util.Play

import scala.jdk.CollectionConverters._


object Main extends App {

  val score = new Score("Row Your Boat")
  val flute = new Part("Flute", FLUTE, 0)
  val trumpet = new Part("Trumpet", TRUMPET, 1)
  val clarinet = new Part("Clarinet", CLARINET, 2)

  val pitchArray = List(C4,C4,C4,D4,E4,E4,D4,E4,F4,G4,
    C5,C5,C5,G4,G4,G4,E4,E4,E4,
    C4,C4,C4,G4,F4,E4,D4,C4)
  val rhythmArray = List(C,C,CT,QT,C,CT,QT,CT, QT,
    M, QT, QT, QT, QT, QT,QT, QT, QT, QT, QT,
    QT, QT, CT, QT, CT, QT,M)

  val notes = for {
    p <- pitchArray
    d <- rhythmArray
  } yield new Note(p, d)

  val phrase1 = new Phrase(0.0)
  for (n <- notes)
    phrase1.add(n)

  val phrase2 = phrase1.copy();
  phrase2.setStartTime(4.0);
  val phrase3 = phrase1.copy();
  phrase3.setStartTime(8.0);

  Mod.transpose(phrase1, 12);
  Mod.transpose(phrase3, -12);

  Mod.repeat(phrase1, 1);
  Mod.repeat(phrase2, 1);
  Mod.repeat(phrase3, 1);

  flute.addPhrase(phrase1)
  trumpet.addPhrase(phrase2)
  clarinet.addPhrase(phrase3)

  score.addPart(flute)
  score.addPart(trumpet)
  score.addPart(clarinet)

  Play.midi(score)

  //  import jm.util.Write
  //  Write.midi(score, "Scale.mid")
  //  val instrument = new SawtoothInst(44100)
  //  Write.au(score, instrument);

}

