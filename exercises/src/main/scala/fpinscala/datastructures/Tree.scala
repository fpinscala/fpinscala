package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(t: Tree[_]): Int = sys.error("todo")

  def maximum(t: Tree[Int]): Int = sys.error("todo")

  def depth(t: Tree[_]): Int = sys.error("todo")

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = sys.error("todo")

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = sys.error("todo")

  def sizeViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold[T](t: Tree[T])(implicit ev: Numeric[T]): T = sys.error("todo")

  def depthViaFold[A](t: Tree[A]): Int = sys.error("todo")
}
