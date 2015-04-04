def map[B](f: A => B): Stream[B] =
  foldRight(empty[B])((h,t) => cons(f(h), t)) 

def filter(f: A => Boolean): Stream[A] =
  foldRight(empty[A])((h,t) => 
    if (f(h)) cons(h, t)
    else t) 

def append[B>:A](s: => Stream[B]): Stream[B] = 
  foldRight(s)((h,t) => cons(h,t))

def flatMap[B](f: A => Stream[B]): Stream[B] = 
  foldRight(empty[B])((h,t) => f(h) append t)
