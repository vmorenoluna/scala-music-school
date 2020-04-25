package music

import cats.syntax.eq._
import music.Types._

sealed trait PrimitiveTypeClass[A] {
  def eqv(x: A, y: A): Boolean

  def neqv(x: A, y: A): Boolean

  def compare(x: A, y: A): Int
}

object PrimitiveTypeClassInstances {

  import PitchClassOps._

  implicit val primitiveInstance: PrimitiveTypeClass[Primitive[Pitch]] = new PrimitiveTypeClass[Primitive[Pitch]] {
    override def eqv(x: Primitive[Pitch], y: Primitive[Pitch]): Boolean = (x, y) match {
      case (a@Note(_, _), b@Note(_, _)) => notePrimitiveInstance.eqv(a, b)
      case (a@Rest(_), b@Rest(_)) => restPrimitiveInstance.eqv(a, b)
      case _ => false
    }

    override def neqv(x: Primitive[Pitch], y: Primitive[Pitch]): Boolean = (x, y) match {
      case (a@Note(_, _), b@Note(_, _)) => notePrimitiveInstance.neqv(a, b)
      case (a@Rest(_), b@Rest(_)) => restPrimitiveInstance.neqv(a, b)
      case _ => false
    }

    override def compare(x: Primitive[Pitch], y: Primitive[Pitch]): Int = (x, y) match {
      case (a@Note(_, _), b@Note(_, _)) => notePrimitiveInstance.compare(a, b)
      case (a@Rest(_), b@Rest(_)) => restPrimitiveInstance.compare(a, b)
    }
  }

  implicit val notePrimitiveInstance: PrimitiveTypeClass[Note[Pitch]] = new PrimitiveTypeClass[Note[Pitch]] {
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

  implicit val restPrimitiveInstance: PrimitiveTypeClass[Rest[Pitch]] = new PrimitiveTypeClass[Rest[Pitch]] {
    override def eqv(x: Rest[Pitch], y: Rest[Pitch]): Boolean =
      x.duration === y.duration

    override def neqv(x: Rest[Pitch], y: Rest[Pitch]): Boolean =
      x.duration =!= y.duration

    override def compare(x: Rest[Pitch], y: Rest[Pitch]): Int =
      x.duration compare y.duration
  }

}

sealed trait MusicTypeClass[A] {
  def eqv(x: A, y: A): Boolean

  def neqv(x: A, y: A): Boolean

  def compare(x: A, y: A): Int
}

object PrimitiveOps {
  final implicit class PrimitiveEqOps[A](private val self: A) {
    def ===(other: A)(implicit p: PrimitiveTypeClass[A]): Boolean = p.eqv(self, other)
    def =!=(other: A)(implicit p: PrimitiveTypeClass[A]): Boolean = p.neqv(self, other)
  }
}

object MusicTypeClassInstances {

  import PrimitiveTypeClassInstances._
  import PrimitiveOps._

  implicit val primMusic: MusicTypeClass[Music[Pitch]] = new MusicTypeClass[Music[Pitch]] {

    override def eqv(x: Music[Pitch], y: Music[Pitch]): Boolean = (x, y) match {
      case (Prim(p1), Prim(p2)) => p1 === p2
      case (Modify(c1, m1), Modify(c2, m2)) => (c1 == c2) && (m1 == m2)
      case (:+:(m1, m2), :+:(m3, m4)) => (m1 == m3) && (m2 == m4)
      case (:=:(m1, m2), :=:(m3, m4)) => (m1 == m3) && (m2 == m4)
      case _ => false
    }

    override def neqv(x: Music[Pitch], y: Music[Pitch]): Boolean = (x, y) match {
      case (Prim(p1), Prim(p2)) => p1 =!= p2
      case (Modify(c1, m1), Modify(c2, m2)) => (c1 != c2) || (m1 != m2)
      case (:+:(m1, m2), :+:(m3, m4)) => (m1 != m3) || (m2 != m4)
      case (:=:(m1, m2), :=:(m3, m4)) => (m1 != m3) || (m2 != m4)
      case _ => false
    }

    override def compare(x: Music[Pitch], y: Music[Pitch]): Int = (x, y) match {
      case (Prim(p1), Prim(p2)) => primitiveInstance.compare(p1, p2)
      case (Prim(p1), _) => -1
      case (Modify(c1, m1), Modify(c2, m2)) => compare(m1, m2) // TODO typeclass for control (c1<c2) || ((c1==c2) && (m1<m2))
      case (Modify(_, m1), _) => 1
      case (:+:(m1, m2), Prim(_)) => 1
      case (:+:(m1, m2), :+:(m3, m4)) if (compare(m1, m3) == -1) || ((m1 == m3) && (compare(m2, m4) == -1)) => -1
      case (:+:(m1, m2), _) => -1
      case (:=:(m1, m2), Prim(_)) => 1
      case (:=:(m1, m2), :=:(m3, m4)) if (compare(m1, m3) == -1) || ((m1 == m3) && (compare(m2, m4) == -1)) => -1
      case (:=:(m1, m2), _) => -1
    }

  }

}
