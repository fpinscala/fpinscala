def chooser[A,B](a: Par[A])(choices: A => Par[B]): Par[B] = 
  es => choices(a(es).get)(es)

/* `chooser` is usually called `flatMap` or `bind`. */
def flatMap[A,B](a: Par[A])(choices: A => Par[B]): Par[B] = 
  es => choices(a(es).get)(es)

def choiceViaFlatMap[A](p: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
  flatMap(p)(b => if (b) ifTrue else ifFalse)

def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
  flatMap(p)(i => choices(i))