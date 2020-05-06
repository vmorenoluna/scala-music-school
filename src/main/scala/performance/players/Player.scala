package performance.players

import music.Music1.Note1
import music.{Accent, Art, Dyn, Legato, Music, NoteAttribute, Params, PhraseAttribute, Staccato, Volume}
import music.Types.{Duration, absPitch}
import performance.{Context, MEvent}
import performance.Performance.{DurT, Performance, perf}

trait Player[A] {
  def pName: String
  def playNote(c: Context[A])(d: Duration)(a: A): Performance
  def interpPhrase(c: Context[A], pas: List[PhraseAttribute], m: Music[A]): (Performance, DurT)
  def getName(): String = s"Player $pName"
}

object Player {
  val players = Map(
    "DefPlayer" -> DefPlayer
  ).withDefaultValue(DefPlayer)
}

object DefPlayer extends Player[Note1] {

  override def pName: String = "DefPlayer"

  override def playNote(c: Context[Note1])(d: Duration)(n: Note1): Performance = {
    val initEv = MEvent(
      eTime = c.cTime,
      eInst = c.cInst,
      ePitch = absPitch(n._1) + c.cPch,
      eDur = d * c.cDur,
      eVol = c.cVol,
      eParams = List.empty
    )
    List(n._2.foldRight(initEv)(nasHandler(c)))
  }

  private def nasHandler(c: Context[Note1])(na: NoteAttribute, ev: MEvent): MEvent =
    (c, na, ev) match {
      case (_, Volume(v), ev) => ev.copy(eVol = v)
      case (_, Params(pms), ev) => ev.copy(eParams = pms)
      case (_, _, ev) => ev
    }

  override def interpPhrase(c: Context[Note1], pas: List[PhraseAttribute], m: Music[Note1]): (Performance, DurT) = {
    val (pf, dur) = perf(c, m)
    (pas.foldRight(pf)(pasHandler), dur)
  }

  private def pasHandler(pa: PhraseAttribute, p: Performance): Performance =
    pa match {
      case Dyn(Accent(x)) => p.map(e => e.copy(eVol = x.intValue * p.head.eVol))
      case Art(Staccato(x)) => p.map(e => e.copy(eDur = x.intValue * p.head.eDur))
      case Art(Legato(x)) => p.map(e => e.copy(eDur = x.intValue * p.head.eDur))
      case _ => p
    }

}
