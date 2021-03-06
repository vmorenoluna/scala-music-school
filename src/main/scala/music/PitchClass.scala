package music

import cats.kernel.{Eq, Order}

sealed trait PitchClass

object PitchClass {
  implicit val pitchClassEq: Eq[PitchClass] = Eq.fromUniversalEquals
  implicit val pitchClassOrder: Order[PitchClass] = Order.from {
    (a, b) => {
      val list = List(
        Cff,
        Cf,
        C,
        Dff,
        Cs,
        Df,
        Css,
        D,
        Eff,
        Ds,
        Ef,
        Fff,
        Dss,
        E,
        Ff,
        Es,
        F,
        Gff,
        Ess,
        Fs,
        Gf,
        Fss,
        G,
        Aff,
        Gs,
        Af,
        Gss,
        A,
        Bff,
        As,
        Bf,
        Ass,
        B,
        Bs,
        Bss
      )
      (list.indexOf(a), list.indexOf(b)) match {
        case (i, j) if i < j => -1
        case (i, j) if i == j => 0
        case (i, j) if i > j => 1
      }
    }
  }
}

object PitchClassOps {
  final implicit class PitchClassOrderOps[A](private val self: A) {
    def gt(other: A)(implicit p: Order[A]): Boolean = p.gt(self, other)
    def lt(other: A)(implicit p: Order[A]): Boolean = p.lt(self, other)
  }
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
