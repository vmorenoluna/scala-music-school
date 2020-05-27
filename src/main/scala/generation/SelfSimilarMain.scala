package generation

import SelfSimilar._
import music._
import music.Music._
import music.Types._
import midi.MidiService
import _root_.music.Music1._
import performance.Performance.hsomPerform
import performance.players.DefPlayer
import performance.{Context, Metronome}

object SelfSimilarMain extends App {

  example1()
  example2()

  private def example1(): Unit = {
    val pattern = LazyList(
      (hn, absPitch((G, 2))),
      (qn, absPitch((A, 2))),
      (hn, absPitch((G, 2))),
      (hn, absPitch((B, 2))),
      (hn, absPitch((C, 2)))
    )

    val music: Music[Pitch] = selfSimilar(pattern, 3, 1)

    val music1: Music1 = music.toMusic1()
    val context: Context[Note1] = Context(0, DefPlayer, AcousticGrandPiano, Metronome.metro(120, qn), 0, 127, (C, Major))
    val performance = hsomPerform(context, music1)

    MidiService.writePerformance(performance, "selfsimilar1.mid")
  }

  private def example2(): Unit = {
    val pattern = LazyList(
      (hn, absPitch((Cs, 3))),
      (qn, absPitch((E, 3))),
      (hn, absPitch((E, 3))),
      (qn, absPitch((Gs, 3))),
      (qn, absPitch((Cs, 3)))
    )

    val m1: Music[Pitch] = selfSimilar(pattern, 2, 1)
    val m2: Music[Pitch] = selfSimilar(pattern.reverse, 2, 1)
    val m3: Music[Pitch] = selfSimilar(pattern.take(2), 2, 1)
    val m = m1 :=: m2

    val music1: Music1 = (m3 :+: m :+: retro(m) :+: m2).toMusic1()
    val context: Context[Note1] = Context(0, DefPlayer, AcousticGrandPiano, Metronome.metro(120, qn), 0, 127, (C, Major))
    val performance = hsomPerform(context, music1)

    MidiService.writePerformance(performance, "selfsimilar2.mid")
  }

}
