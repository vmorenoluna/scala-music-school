package music

import cats.syntax.eq._
import PitchClassOps._
import music.Types._

sealed trait PrimitiveTypeClass[A] {
  def eqv(x: A, y: A): Boolean
  def neqv(x: A, y: A): Boolean
  def compare(x: A, y: A): Int
}

object PrimitiveTypeClassInstances {

  implicit val notePrimitive: PrimitiveTypeClass[Note[Pitch]] = new PrimitiveTypeClass[Note[Pitch]] {
    override def eqv(x: Note[Pitch], y: Note[Pitch]): Boolean =
      x.duration === y.duration && x.features === y.features

    override def neqv(x: Note[Pitch], y: Note[Pitch]): Boolean =
      x.duration =!= y.duration || x.features =!= y.features

    override def compare(x: Note[Pitch], y: Note[Pitch]): Int = (x, y) match {
      case (Note(d1, p1), Note(d2, p2)) if (p1 gt p2) || (p1 === p2 && d1 > d2) => 1
      case (Note(d1, p1), Note(d2, p2)) if p1 === p2 && d1 === d2 => 0
      case (Note(d1, p1), Note(d2, p2)) if (p1 lt p2) || (p1 === p2 && d1 < d2) => -1
    }
  }

  implicit val restPrimitive: PrimitiveTypeClass[Rest[Pitch]] = new PrimitiveTypeClass[Rest[Pitch]] {
    override def eqv(x: Rest[Pitch], y: Rest[Pitch]): Boolean =
      x.duration === y.duration

    override def neqv(x: Rest[Pitch], y: Rest[Pitch]): Boolean =
      x.duration =!= y.duration

    override def compare(x: Rest[Pitch], y: Rest[Pitch]): Int =
      x.duration compare y.duration
  }

}

object PrimitiveOps {

  final implicit class PrimitiveEqOps[A](private val self: A) {
    def ===(other: A)(implicit p: PrimitiveTypeClass[A]): Boolean = p.eqv(self, other)
    def =!=(other: A)(implicit p: PrimitiveTypeClass[A]): Boolean = p.neqv(self, other)
  }

}
