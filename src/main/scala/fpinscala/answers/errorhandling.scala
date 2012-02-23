package fpinscala.answers

trait Option[+A] {

  def map[B](f: A => B): Option[B] = 
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
    
  def getOrElse[B>:A](default: => B): B = 
    this match {
      case None => default
      case Some(a) => a
    }
    
  def flatMap[B](f: A => Option[B]): Option[B] = 
    this map f getOrElse None
    
} 
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]
  
