package performance

import music.InstrumentName
import music.Types.{AbsPitch, Volume}
import performance.types.{DurT, PTime}
import spire.math.Rational

case class MMEvent(
                    eTime: PTime,           // onset time
                    eInst: InstrumentName,  // assigned instrument
                    ePitch: AbsPitch,       // pitch number 0-127
                    eDur: DurT,             // note duration
                    eVol: Volume,           // volume 0-127
                    eParams: List[Double]   // optional other parameters
                  )

// TODO Eq and Ord typeclass for MMEvent

object types {
  type Performance = List[MMEvent]
  type PTime = Rational
  type DurT = Rational
}
