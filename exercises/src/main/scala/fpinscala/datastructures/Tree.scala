package fpinscala.datastructures


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  /**
    * Exercise 3.25
    *
    * Write a function size that counts the number of nodes (leaves and branches) in a tree.
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r) // count this node, too
  }

  /**
    * Exercise 3.26
    *
    * Write a function maximum that returns the maximum element in a Tree[Int]. (Note: In Scala, you
    * can use x.max(y) or x max y to compute the maximum of two integers x andy.)
    */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n)      => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  /**
    * Exercise 3.27
    *
    * Write a function depth that returns the maximum path length from the root of a tree to any
    * leaf.
    */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  /**
    * Exercise 3.28
    *
    * Write a function map, analogous to the method of the same name on List, that modifies each
    * element in a tree with a given function.
    */
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n)      => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  /**
    * Exercise 3.29
    *
    * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over
    * their similarities. Reimplement them in terms of this more general function. Can you draw an
    * analogy between this fold function and the left and right folds for List?
    *
    * ==Note==
    * Similarly to foldRight (and foldLeft) for lists, this fold provides a "handler" for each of
    * the data constructors of its ADT. So, if we were to supply those constructors as parameters to
    * fold, we'd get the input tree back, as below:
    *
    * {{{
    *   scala> val t: Tree[Int] = Branch(Leaf(1), Leaf(2))
    *   t: fpinscala.datastructures.Tree[Int] = Branch(Leaf(1),Leaf(2))
    *
    *   scala> fold(t)(i => Leaf(i): Tree[Int])(Branch(_, _))
    *   res1: fpinscala.datastructures.Tree[Int] = Branch(Leaf(1),Leaf(2))
    * }}}
    */
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(n)      => f(n)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ + _)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(identity)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((x, y) => 1 + (x max y))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}
