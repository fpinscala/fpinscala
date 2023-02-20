package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = ???

  /*
  We're using the method `max` that exists on all `Int` values rather than an explicit `if` expression.

  Note how similar the implementation is to `size`. We'll abstract out the common pattern in a later exercise.
  */
  def maximum(t: Tree[Int]): Int = ???

  /*
  Again, note how similar the implementation is to `size` and `maximum`.
  */
  def depth[A](t: Tree[A]): Int = ???

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = ???

  /*
  Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively
  accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
  this function to implement just about any recursive function that would otherwise be defined by pattern matching.
  */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = ???

  def sizeViaFold[A](t: Tree[A]): Int =
    ???

  def maximumViaFold(t: Tree[Int]): Int =
    ???

  def depthViaFold[A](t: Tree[A]): Int =
    ???

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    ???
}
