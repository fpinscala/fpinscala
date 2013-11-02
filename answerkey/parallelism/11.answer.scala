def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = 
  es => {
    val ind = run(es)(n).get // Notice we are blocking on the result of `cond`.
    run(es)(choices(ind))
  }

def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
  choiceN(map(a)(b => if (b) 1 else 0))(List(ifTrue, ifFalse))