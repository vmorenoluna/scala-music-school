package performance

import music.InstrumentName
import music.Types.{AbsPitch, Volume}
import performance.Performance.{DurT, PTime}

case class MEvent(
                    eTime: PTime,           // onset time
                    eInst: InstrumentName,  // assigned instrument
                    ePitch: AbsPitch,       // pitch number 0-127
                    eDur: DurT,             // note duration
                    eVol: Volume,           // volume 0-127
                    eParams: List[Double]   // optional other parameters
                  )

// TODO Eq and Ord typeclass for MMEvent


