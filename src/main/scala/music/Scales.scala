package music

import music.Music._
import music.Types._

object Scales {

  def wholeToneScale(p: Pitch): List[Music[Pitch]] =
    (0 to 8 by 2).toList.map(
      ap => note(qn, pitch(absPitch(p) + ap))
    )

  def chromaticScale(ap1: AbsPitch, ap2: AbsPitch): List[Music[Pitch]] =
    (ap1 to ap2 by 1).toList.map(
      ap => note(qn, pitch(ap))
    )

  def chromaticScale(p1: Pitch, p2: Pitch): Music[Pitch] =
    (absPitch(p1), absPitch(p2)) match {
      case (ap1, ap2) if ap1 == ap2 => note(qn, p1)
      case (ap1, ap2) => line(chromaticScale(ap1, ap2))
    }

  def makeScale(p: Pitch, pattern: List[Step]): Music[Pitch] = {
    val absScale: List[AbsPitch] = pattern.scanLeft(absPitch(p)) {
      case(p1,p2) => p1 + p2
    }

    line (for {
            ap <- absScale
            p = pitch(ap)
            n = note(qn, p)
          } yield n
    )
  }

  def genScale(mode: Mode): List[Step] = {
    // val pattern: Stream[Step] = List(ws, ws, hs, ws, ws, ws, hs).toStream #::: pattern
    mode match {
      case Major | Ionian => List(ws, ws, hs, ws, ws, ws, hs)
      case Dorian => List(ws, hs, ws, ws, ws, hs, ws)
      case Phrygian => List(hs, ws, ws, ws, hs, ws, ws)
      case Lydian => List(ws, ws, ws, hs, ws, ws, hs)
      case Mixolydian => List(ws, ws, hs, ws, ws, hs, ws)
      case Minor | Aeolian => List(ws, hs, ws, ws, hs, ws, ws)
      case Locrian => List(hs, ws, ws, hs, ws, ws, ws)
      case CustomMode(_) => List.empty
    }

  }

}
