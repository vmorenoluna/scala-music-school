package map

import music._
import music.Types._
import music.Music._

object Functions {

  def f1(n: Int, ps: List[Pitch]): List[(PitchClass, Octave)] = ps.map(trans(n, _))

  def f2(ds: List[Duration]): List[Music[Nothing]] = ds.map(rest)

  def f3(mps: List[Music[Pitch]]): List[Music[Pitch]] = mps match {
    case Nil => Nil
    case ::(head, tail) => staccato(head) :: f3(tail)
  }

  def staccato(note: Music[Pitch]): Music[Pitch] = note match {
    case Prim(Note(d, pitch)) => Prim(Note(d / 2, pitch))
    case Prim(Rest(d)) => Prim(Rest(d))
    case Modify(control, m) => Modify(control, staccato(m))
    case m1 :+: m2 => staccato(m1) :+: staccato(m2)
    case m1 :=: m2 => staccato(m1) :=: staccato(m2)
  }

  def applyEach[A, B](functions: List[A => B], value: A): List[B] =
    functions.map(f => f(value))

  def applyAll[A](functions: List[A => A], value: A): A =
    functions.foldRight(value)((f1, f2) => f1(f2))

  def doubleEach(ns: List[Int]): List[Int] =
    ns.map(_ * 2)

  def pairAndOne(ns: List[Int]): List[(Int, Int)] =
    ns.map(n => (n, n + 1))

  def addEachPair(pairs: List[(Int, Int)]): List[Int] =
    pairs.map(p => p._1 + p._2)

  def addPairPointwiseUnzip(pairs: List[(Int, Int)]): (Int, Int) =
    pairs.unzip match {
      case (l1, l2) => (l1.sum, l2.sum)
    }

  def addPairPointwiseFoldLeft(pairs: List[(Int, Int)]): (Int, Int) =
    pairs.foldLeft((0, 0)) { case ((accA, accB), (a, b)) => (accA + a, accB + b) }

  def prefixes[A](list: List[A]): List[List[A]] = list match {
    case Nil => Nil
    case ::(h, t) => List(h) :: prefixes(t).map(h :: _)
  }

  def twice[A](f: A => A)(a: A): A =
    f(f(a))

  def power[A](f: A => A, n: Int)(a: A): A = n match {
    case 0 => a
    case i => power(f, i - 1)(f(a))
  }

  def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }

  val remainder: Int => Int => Int = x => y =>
    (x, y) match {
      case (x, y) if x < y => x
      case (x, y) => remainder(x - y)(y)
    }

  val remainderWithFix =
    fix[Int => Int => Int](remainder => x => y => if (x < y) x else remainder(x-y)(y))

}
