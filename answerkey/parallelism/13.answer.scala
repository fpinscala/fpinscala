extension [A](pa: Par[A]) def chooser[B](choices: A => Par[B]): Par[B] =
  es => {
    val k = pa.run(es).get
    choices(k).run(es)
  }

/* `chooser` is usually called `flatMap` or `bind`. */
extension [A](pa: Par[A]) def flatMap[B](choices: A => Par[B]): Par[B] =
  es => {
    val a = pa.run(es).get
    choices(a).run(es)
  }

def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
  p.flatMap(b => if b then t else f)

def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
  p.flatMap(i => choices(i))
