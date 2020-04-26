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
      case (Note(d1, p1), Note(d2, p2)) => d1 === d2 && p1 === p2
      case (Rest(d1), Rest(d2)) => d1 === d2
      case _ => false
    }
    override def neqv(x: Primitive[Pitch], y: Primitive[Pitch]): Boolean = (x, y) match {
      case (Note(d1, p1), Note(d2, p2)) => (d1 =!= d2) || (p1 =!= p2)
      case (Rest(d1), Rest(d2)) => d1 =!= d2
      case _ => false
    }
    override def compare(x: Primitive[Pitch], y: Primitive[Pitch]): Int = (x, y) match {
      case (Note(d1, p1), Note(d2, p2)) if (p1 gt p2) || (p1 === p2 && d1 > d2) => 1
      case (Note(d1, p1), Note(d2, p2)) if p1 === p2 && d1 === d2 => 0
      case (Note(d1, p1), Note(d2, p2)) if (p1 lt p2) || (p1 === p2 && d1 < d2) => -1
      case (Rest(d1), Rest(d2)) => d1 compare d2
      case (Rest(_), Note(_, _)) => -1
      case (Note(_, _), Rest(_)) => 1
    }
  }

}

object PrimitiveOps {
  final implicit class PrimitiveEqOps[A](private val self: A) {
    def ===(other: A)(implicit p: PrimitiveTypeClass[A]): Boolean = p.eqv(self, other)
    def =!=(other: A)(implicit p: PrimitiveTypeClass[A]): Boolean = p.neqv(self, other)
  }
}

sealed trait MusicTypeClass[A] {
  def eqv(x: A, y: A): Boolean
  def neqv(x: A, y: A): Boolean
  def compare(x: A, y: A): Int
}

object MusicTypeClassInstances {

  import PrimitiveTypeClassInstances._
  import PrimitiveOps._

  implicit val musicInstance: MusicTypeClass[Music[Pitch]] = new MusicTypeClass[Music[Pitch]] {

    override def eqv(x: Music[Pitch], y: Music[Pitch]): Boolean = (x, y) match {
      case (Prim(p1), Prim(p2)) => p1 === p2
      case (Modify(c1, m1), Modify(c2, m2)) => (c1 == c2) && eqv(m1, m2)
      case (:+:(m1, m2), :+:(m3, m4)) => eqv(m1, m3) && eqv(m2, m4)
      case (:=:(m1, m2), :=:(m3, m4)) => eqv(m1, m3) && eqv(m2, m4)
      case _ => false
    }

    override def neqv(x: Music[Pitch], y: Music[Pitch]): Boolean = (x, y) match {
      case (Prim(p1), Prim(p2)) => p1 =!= p2
      case (Modify(c1, m1), Modify(c2, m2)) => (c1 != c2) || neqv(m1, m2)
      case (:+:(m1, m2), :+:(m3, m4)) => neqv(m1, m3) || neqv(m2, m4)
      case (:=:(m1, m2), :=:(m3, m4)) => neqv(m1, m3) || neqv(m2, m4)
      case _ => false
    }

    override def compare(x: Music[Pitch], y: Music[Pitch]): Int = (x, y) match {
      case (Prim(p1), Prim(p2)) => primitiveInstance.compare(p1, p2)
      case (Prim(_), _) => -1
      case (:+:(_, _), Prim(_)) => 1
      case (:+:(m1, m2), :+:(m3, m4)) if (compare(m1, m3) == -1) || (compare(m1, m3) == 0 && compare(m2, m4) == -1) => -1
      case (:+:(m1, m2), :+:(m3, m4)) if (compare(m1, m3) == 0) && (compare(m2, m4) == 0) => 0
      case (:+:(m1, m2), :+:(m3, m4)) if (compare(m1, m3) == 1) || (compare(m1, m3) == 0 && compare(m2, m4) == 1) => 1
      case (:+:(_, _), _) => -1
      case (:=:(_, _), Prim(_)) => 1
      case (:=:(m1, m2), :=:(m3, m4)) if (compare(m1, m3) == -1) || (compare(m1, m3) == 0 && compare(m2, m4) == -1) => -1
      case (:=:(m1, m2), :=:(m3, m4)) if (compare(m1, m3) == 0) && (compare(m2, m4) == 0) => 0
      case (:=:(m1, m2), :=:(m3, m4)) if (compare(m1, m3) == 1) || (compare(m1, m3) == 0 && compare(m2, m4) == 1) => 1
      case (:=:(_, _), _) => -1
      case (Modify(_, m1), Modify(_, m2)) => compare(m1, m2) // TODO typeclass for control (c1<c2) || ((c1==c2) && (m1<m2))
      case (Modify(_, _), _) => 1
    }

  }

}
