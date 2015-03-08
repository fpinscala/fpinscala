package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match{
    case Leaf(_) => 1
    case Branch(l,r) => 1+size(l)+size(r)
  }
  
  def maximum(t: Tree[Int]): Int = t match{
    case Leaf(e) => e
    case Branch(l,r) => maximum(l) max maximum(r)
  }
  
  def depth[A](t: Tree[A]): Int = t match{
    case Leaf(_) => 1
    case Branch(l,r) => (1+depth(l)) max (1+depth(r))
  }
  
  def map[A,B](t: Tree[A])(f: A=>B): Tree[B] = t match {
    case Leaf(l) => Leaf(f(l))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = sys.error("todo")

  def sizeViaFold[A](t: Tree[A]): Int = 
    fold(t)(a => 1)(1 + _ + _)

  def maximumViaFold[T](t: Tree[T])(implicit ev: Numeric[T]): T = sys.error("todo")

  def depthViaFold[A](t: Tree[A]): Int = sys.error("todo")
  
}

object TreeTest {
   def main(args: Array[String]): Unit = {
    val t = Branch(Branch(Leaf(1),Branch(Leaf(1),Leaf(2))), Leaf(500))
    println("t: "+t.toString)
    println("Size of t: "+Tree.size(t).toString)
    println("Max of t: "+Tree.maximum(t).toString)
    println("Depth of t: "+Tree.depth(t).toString)
    println("T minus 1: "+Tree.map(t)(_ - 1).toString)
   }
}