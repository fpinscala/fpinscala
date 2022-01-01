package fpinscala.answers.streamingio

import language.postfixOps

/* Data type representing either A, B, or both A and B. */
trait These[+A,+B] {
  import These.*

  def bimap[A2,B2](f: A => A2, g: B => B2): These[A2,B2] = 
    this match {
      case This(a) => This(f(a)) 
      case That(b) => That(g(b)) 
      case Both(a,b) => Both(f(a), g(b)) 
    }

  def mapL[A2,B2>:B](f: A => A2): These[A2,B2] = 
    bimap(f, identity)

  def mapR[A2>:A,B2](f: B => B2): These[A2,B2] = 
    bimap(identity, f)

  def isBoth = this match {
    case Both(_,_) => true
    case _ => false
  }
}

object These {
  case class This[+A](a: A) extends These[A,Nothing]
  case class That[+B](b: B) extends These[Nothing,B]
  case class Both[+A,+B](a: A, b: B) extends These[A,B]

  def zipAll[A,B,C](a: Seq[A], b: Seq[B]): LazyList[These[A,B]] = 
    if (a isEmpty) b.to(LazyList).map(That(_))
    else if (b isEmpty) a.to(LazyList).map(This(_))
    else Both(a.head, b.head) #:: zipAll(a.tail, b.tail)
  
  /* 
   * Zips together the two `Seq`s, returning the remaining elemnts 
   * of each (possibly empty). 
   */
  def zipResidual[A,B,C](a: Seq[A], b: Seq[B]): 
     (Seq[(A,B)], Seq[A], Seq[B]) = {
    val z = a zip b
    val len = z.length
    (z, a drop len, b drop len)
  }
}
