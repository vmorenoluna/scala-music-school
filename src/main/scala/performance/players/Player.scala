package performance.players

import music.{NoteAttribute, Params, Volume}
import music.Types.{Duration, Pitch, absPitch}
import performance.{Context, MEvent}
import performance.Performance.{NoteFun, PhraseFun, PlayerName}

trait Player[A] {
  def pName: PlayerName

  def playNote: NoteFun[A]

  def interpPhrase: PhraseFun[A]

  def getName(): String = s"Player $pName"
}

case class DefPlayer[A]() extends Player[A] {

  override def pName: PlayerName = "DefPlayer"

  override def playNote: NoteFun[A] = ???

  private val defPlayNote = (nasHandler: Context[(Pitch, List[A])] => (A, MEvent) => MEvent) =>
    (c: Context[(Pitch, List[A])]) => (d: Duration) => (p: Pitch, nas: List[A]) => {
      val initEv = MEvent(
        eTime = c.cTime,
        eInst = c.cInst,
        ePitch = absPitch(p) + c.cPch,
        eDur = d * c.cDur,
        eVol = c.cVol,
        eParams = List.empty
      )
      nas.foldRight(initEv)(nasHandler(c))
  }

  private val defNasHandler = (c: Context[A]) => (na: NoteAttribute, ev: MEvent) =>
    (c, na, ev) match {
      case (_, Volume(v), ev) => ev.copy(eVol = v)
      case (_, Params(pms), ev) => ev.copy(eParams = pms)
      case (_, _, ev) => ev
    }

  override def interpPhrase: PhraseFun[A] = ???

}
