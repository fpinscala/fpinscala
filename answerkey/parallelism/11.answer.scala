def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = 
  es => {
    val ind = run(es)(n).get // Full source files
    run(es)(choices(ind))
  }

def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
  choiceN(map(a)(b => if (b) 0 else 1))(List(ifTrue, ifFalse))