package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => max(l).max(max(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(c: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => c(fold(l)(f)(c), fold(r)(f)(c))
  }

  def size2[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def max2(t: Tree[Int]): Int = fold[Int, Int](t)(identity)(_ max _)

  def depth2[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 0)(1 + _ max _)

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(v => Leaf(f(v)))(Branch(_, _))

  def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    println(size(t))
    println(size2(t))
    println()

    println(max(t))
    println(max2(t))
    println()

    println(depth(t))
    println(depth2(t))
    println()

    println(map(t)(_ + 1))
    println(map2(t)(_ + 1))
  }

}