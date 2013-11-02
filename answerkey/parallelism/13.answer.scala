def chooser[A,B](p: Par[A])(choices: A => Par[B]): Par[B] = 
  es => {
    val k = run(es)(p).get
    run(es)(choices(k))
  }

/* `chooser` is usually called `flatMap` or `bind`. */
def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] = 
  es => {
    val k = run(es)(p).get
    run(es)(choices(k))
  }

def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
  flatMap(p)(b => if (b) t else f)

def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
  flatMap(p)(i => choices(i))