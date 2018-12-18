package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Counts the number of nodes (leaves and branches) in a tree
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // The maximum element in a Tree[Int]
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l).max(maximum(r))
  }

  // The maximum path length from the root of a tree to any leaf
  def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
  }

  // Analogous to map() on List
  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](tree: Tree[A])(z: A => B)(f: (B, B) => B): B = tree match {
    case Leaf(v) => z(v)
    case Branch(l, r) => f( fold(l)(z)(f) , fold(r)(z)(f) )
  }

  // write below functions  using fold

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def maximum2(tree: Tree[Int]): Int = fold(tree)(identity)(_.max(_))

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 0)(_.max(_) + 1)

  def map2[A,B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)((a: A) => Leaf(f(a)): Tree[B])(Branch(_, _))
}
