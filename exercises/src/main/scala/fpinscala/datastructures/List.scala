package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = 
    l match{
      case Cons(h, t) => t
      case Nil => Nil
    }

  def setHead[A](l: List[A], h: A): List[A] = 
    l match {
      case Cons(a, as) => Cons(h, as)
      case Nil => Cons(h, Nil) //little weird, but we are asking to set the head...
    }

  def drop[A](l: List[A], n: Int): List[A] = 
    if(n < 1)
      l
    else
      l match{
        case Cons(h, t) => drop(t, n - 1)
        case Nil => Nil
      }
  
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = 
    l match{
      case Cons(h, t) => if(f(h)) dropWhile(t, f) else l
      case Nil => Nil
    }

  def init[A](l: List[A]): List[A] = 
    l match{
      case Cons(h0, Cons(h1, Nil)) => Cons(h0, Nil)
      case Cons(h, Nil) => Nil
      case Nil => Nil
      case Cons(h, l) => Cons(h, init(l))
    }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((x, a:Int) => a+1)
  
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match{
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }
  }
  def sum3(ns: List[Int]) = 
    foldLeft(ns, 0)((x,y) => x + y)
  
  def product3(ns: List[Double]) = 
    foldLeft(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  
  def length2[A](l: List[A]): Int = 
    foldLeft(l, 0)((a:Int, x) => a+1)
    
  
  def reverse[A](l: List[A]):List[A] = {
    foldLeft(l, Nil:List[A])((acc, h) => Cons(h,acc))
  }
  
  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b,a)=>f(a,b))
 
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRightViaFoldLeft(a1, a2)((h, t) => Cons(h,t))
  
  def concat[A](ll: List[List[A]]):List[A] = {
    foldRightViaFoldLeft(ll, Nil:List[A])(append2)
  }
  
  def addone(li: List[Int]) = 
    foldLeft(li, Nil:List[Int])((t,h) => Cons(h+1,t))
   
  def dlist2strlst(ld: List[Double]):List[String] =
    foldLeft(ld, Nil:List[String])((t,d) => Cons(d.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] = 
    foldRightViaFoldLeft(l, Nil:List[B])((a,b) => Cons(f(a), b))
  
  def filter[A](as: List[A])(f: A=>Boolean):List[A] = {
    as match {
      case Cons(h,t) => if(f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
      case Nil => Nil 
    }
  }
  def flatMap[A,B](as:List[A])(f: A=>List[B]):List[B] = {
    foldRightViaFoldLeft(as, Nil:List[B])((a,b) => append2(f(a),b))
  }
  
  def filterViaFlatMap[A](as: List[A])(f: A=>Boolean):List[A] = {
    flatMap(as)((a:A) => if(f(a)) List(a) else Nil:List[A])
  }
  
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    as match {
      case Cons(ha, ta) => bs match {
        case Cons(hb, tb) => Cons(f(ha,hb), zipWith(ta, tb)(f))
        case Nil => Nil
      }
      case Nil => Nil
    }
  }
  
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def go(p:List[A], b:List[A]) : Boolean = {
      b match {
        case Nil => true
        case Cons(hb, tb) => p match {
          case Nil => false
          case Cons(hp, tp) =>
            if(hb == hp) go(tp, tb) //always advance through sup
            else go(tp, sub) // restore original sub on mismatch
        }
      }
    }
    go(sup,sub)
  }
}