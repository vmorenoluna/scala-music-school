package music

import cats.kernel.Eq

sealed trait PitchClass

object PitchClass {
  implicit val pitchClassEq: Eq[PitchClass] = Eq.fromUniversalEquals
}

final case object Cff extends PitchClass
final case object Cf extends PitchClass
final case object C extends PitchClass
final case object Dff extends PitchClass
final case object Cs extends PitchClass
final case object Df extends PitchClass
final case object Css extends PitchClass
final case object D extends PitchClass
final case object Eff extends PitchClass
final case object Ds extends PitchClass
final case object Ef extends PitchClass
final case object Fff extends PitchClass
final case object Dss extends PitchClass
final case object E extends PitchClass
final case object Ff extends PitchClass
final case object Es extends PitchClass
final case object F extends PitchClass
final case object Gff extends PitchClass
final case object Ess extends PitchClass
final case object Fs extends PitchClass
final case object Gf extends PitchClass
final case object Fss extends PitchClass
final case object G extends PitchClass
final case object Aff extends PitchClass
final case object Gs extends PitchClass
final case object Af extends PitchClass
final case object Gss extends PitchClass
final case object A extends PitchClass
final case object Bff extends PitchClass
final case object As extends PitchClass
final case object Bf extends PitchClass
final case object Ass extends PitchClass
final case object B extends PitchClass
final case object Bs extends PitchClass
final case object Bss extends PitchClass
