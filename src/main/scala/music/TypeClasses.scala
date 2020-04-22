package music

import cats.syntax.eq._
import music.Types._

sealed trait PrimitiveTypeClass[A] {
  def eqv(x: A, y: A): Boolean

  def neqv(x: A, y: A): Boolean
}

object PrimitiveTypeClassInstances {

  implicit val notePrimitive: PrimitiveTypeClass[Note[Pitch]] = new PrimitiveTypeClass[Note[Pitch]] {
    override def eqv(x: Note[Pitch], y: Note[Pitch]): Boolean =
      x.duration === y.duration && x.features === y.features

    override def neqv(x: Note[(PitchClass, Octave)], y: Note[(PitchClass, Octave)]): Boolean =
      x.duration =!= y.duration || x.features =!= y.features
  }
  implicit val restPrimitive: PrimitiveTypeClass[Rest[Pitch]] = new PrimitiveTypeClass[Rest[Pitch]] {
    override def eqv(x: Rest[Pitch], y: Rest[Pitch]): Boolean =
      x.duration === y.duration

    override def neqv(x: Rest[(PitchClass, Octave)], y: Rest[(PitchClass, Octave)]): Boolean =
      x.duration =!= y.duration
  }
}

object PrimitiveOps {

  final implicit class PrimitiveEqOps[A](private val self: A) extends AnyVal {
    def ===(other: A)(implicit p: PrimitiveTypeClass[A]): Boolean = p.eqv(self, other)
    def =!=(other: A)(implicit p: PrimitiveTypeClass[A]): Boolean = p.neqv(self, other)
  }

}
