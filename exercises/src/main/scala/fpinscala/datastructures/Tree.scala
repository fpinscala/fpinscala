package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])( f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size1[A](t: Tree[A]): Int = fold(t)(_ => 1)(1 + _ + _)

  def maximum1(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depth1[A](t: Tree[A]): Int = fold(t)(_ => 1)((d1, d2) => 1 + (d1 max d2))

  def map1[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}