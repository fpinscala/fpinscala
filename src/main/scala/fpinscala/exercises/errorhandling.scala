package fpinscala.exercises

trait Option[+A] {

  def map[B](f: A => B): Option[B] = 
    sys.error("todo")

  def getOrElse[B>:A](default: => B): B = 
    sys.error("todo")

  def flatMap[B](f: A => Option[B]): Option[B] = 
    sys.error("todo")


} 
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]
