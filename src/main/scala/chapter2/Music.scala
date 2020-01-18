package chapter2

import chapter2.Types.Duration

sealed trait Primitive[A]
final case class Note[A](duration: Duration, features: A) extends Primitive[A]
final case class Rest[A](duration: Duration) extends Primitive[A]

sealed trait Music[A] {
  def :+:(that: Music[A]): Music[A] = ???  // TODO sequential composition
  def :=:(that: Music[A]): Music[A] = ???  // TODO parallel composition
  def modify[A](control: Control): Music[A] = ???
}
final case class Prim[A](primitive: Primitive[A]) extends Music[A]
