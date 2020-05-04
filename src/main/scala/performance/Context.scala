package performance

import music.{InstrumentName, Mode, PitchClass}
import music.Types.{AbsPitch, Volume}
import performance.Performance.{DurT, PTime}
import performance.players.Player

case class Context[A](
                  cTime: PTime,
                  cPlayer: Player[A],
                  cInst: InstrumentName,
                  cDur: DurT,
                  cPch: AbsPitch,
                  cVol: Volume,
                  cKey: (PitchClass, Mode)
                  )

