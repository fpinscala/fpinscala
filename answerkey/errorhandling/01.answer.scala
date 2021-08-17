def map[B](f: A => B): Option[B] = this match
  case None => None
  case Some(a) => Some(f(a))

def getOrElse[B>:A](default: => B): B = this match
  case None => default
  case Some(a) => a

def flatMap[B](f: A => Option[B]): Option[B] = 
  map(f).getOrElse(None)

/*
Of course, we can also implement `flatMap` with explicit pattern matching.
*/
def flatMap_1[B](f: A => Option[B]): Option[B] = this match
  case None => None
  case Some(a) => f(a)

def orElse[B>:A](ob: => Option[B]): Option[B] = 
  map(Some(_)).getOrElse(ob)

/*
Again, we can implement this with explicit pattern matching. 
*/
def orElse_1[B>:A](ob: => Option[B]): Option[B] = this match
  case None => ob 
  case _ => this

/*
This can also be defined in terms of `flatMap`.
*/
def filter_1(f: A => Boolean): Option[A] =
  flatMap(a => if (f(a)) Some(a) else None)

/* Or via explicit pattern matching. */  
def filter(f: A => Boolean): Option[A] = this match
  case Some(a) if f(a) => this
  case _ => None
