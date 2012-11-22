package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = sys.error("todo")

  def maximum(t: Tree[Int]): Int = sys.error("todo")

  def depth[A](t: Tree[A]): Int = sys.error("todo")

  def map[A, B](t: Tree[A])(f: A ⇒ B): Tree[B] = sys.error("todo")

  def fold[A, B](t: Tree[A])(l: Leaf[A] ⇒ Leaf[B], b: Branch[A] ⇒ Branch[B]): Tree[B] = sys.error("todo")

}
