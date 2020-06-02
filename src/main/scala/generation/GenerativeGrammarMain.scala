package generation

import generation.GenerativeGrammar._
import generation.MusicGrammar._
import midi.MidiService
import music.{AcousticGrandPiano, C, Major, Music}
import music.Types.{Pitch, transM}
import music.Music._
import music.Music1._
import performance.{Context, Metronome}
import performance.Performance.hsomPerform
import performance.players.DefPlayer

object GenerativeGrammarMain extends App {

  sealed trait LFun
  final case class Inc() extends LFun
  final case class Dec() extends LFun
  final case class Same() extends LFun

  val ir: IR[LFun, Pitch] = List(
    (Inc(), transM(1, _)),
    (Dec(), transM(-1, _)),
    (Same(), transM(0, _))
  )

  val inc: LSys[LFun] = N(Inc())
  val dec: LSys[LFun] = N(Dec())
  val same: LSys[LFun] = N(Same())

  val sc: LSys[LFun] = :+(inc, dec)   // TODO operator

  val r1a: Rule[LSys[LFun]] = Rule(inc, ::(sc, sc))
  val r1b: Rule[LSys[LFun]] = Rule(inc, sc)
  val r2a: Rule[LSys[LFun]] = Rule(dec, ::(sc, sc))
  val r2b: Rule[LSys[LFun]] = Rule(dec, sc)
  val r3a: Rule[LSys[LFun]] = Rule(same, inc)
  val r3b: Rule[LSys[LFun]] = Rule(same, dec)
  val r3c: Rule[LSys[LFun]] = Rule(same, same)

  val g1: Grammar[LSys[LFun]] = Grammar(same, Uni(List(r1b,r1a,r2b,r2a,r3a,r3b,r3c)))

  val music: Music[Pitch] = interpret(
    gen(replFun[LFun], g1, 42)(6),
    ir,
    c(5)(en)
  )

  val music1: Music1 = music.toMusic1()
  val context: Context[Note1] = Context(0, DefPlayer, AcousticGrandPiano, Metronome.metro(120, qn), 0, 127, (C, Major))
  val performance = hsomPerform(context, music1)

  MidiService.writePerformance(performance, "GenerativeGrammar1.mid")

}