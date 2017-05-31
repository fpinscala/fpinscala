package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = fold(t)(a => 1)((b, b2) => b + b2)

  def max(t: Tree[Int]): Int = fold(t)(a => a)((b, b2) => b max b2)

  def depth[A](t: Tree[A]): Int = fold(t)(a => 1)((b, b2) => 1 + (b max b2))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])((b, b2) => Branch(b, b2))

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(a) => l(a)
    case Branch(left, right) => b(fold(left)(l)(b), fold(right)(l)(b))
  }
}