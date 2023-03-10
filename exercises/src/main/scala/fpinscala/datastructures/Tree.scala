package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 3.25: Write a function `size` that counts the number of nodes (leaves and branches) in a tree

  // Exercise 3.26: Write a function `maximum` that returns the maximum element in a `Tree[Int]`

  // Exercise 3.27: Write a function `depth` that returns the maximum path length from the root of a tree to any leaf

  // Exercise 3.28: Write a function `map`, analogous to the method of the same name on `List`, that modifies each element
  // in a tree with a given function

  // Exercise 3.29: Generalize `size`, `maximum`, `depth`, and `map`, writing a new function `fold` that abstracts over
  // their similarities. Reimplement them in terms of this more general function
}