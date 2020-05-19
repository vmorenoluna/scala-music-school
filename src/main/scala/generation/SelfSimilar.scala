package generation

import music.Music
import music.Music._
import music.Types._

object SelfSimilar {

  type SNote = (Duration, AbsPitch)

  case class Cluster(note: SNote, children: LazyList[Cluster])

  def selfSimilar(pattern: LazyList[SNote], level: Int, trans: AbsPitch): Music[Pitch] =
    transM(trans,
      simToMusic(
        fringe(level, selfSim(pattern))
      )
    )

  private def selfSim(pat: LazyList[SNote]): Cluster = {
    def mkCluster(note: SNote): Cluster =
      Cluster(note, pat.map(n =>
        mkCluster(addMult(note, n)))
      )

    Cluster((0, 0), pat.map(mkCluster(_)))
  }

  private def addMult(n0: SNote, n1: SNote): SNote =
    (n0._1 * n1._1, n0._2 + n1._2)

  private def fringe(level: Int, cluster: Cluster): List[SNote] = (level, cluster) match {
    case (0, Cluster(note, _)) => List(note)
    case (n, Cluster(_, cls)) => cls.flatMap(fringe(n - 1, _)).toList
  }

  private def simToMusic(notes: List[SNote]): Music[Pitch] =
    line(notes.map(mkNote))

  private def mkNote(snote: SNote): Music[Pitch] =
    note(snote._1, pitch(snote._2))

}
