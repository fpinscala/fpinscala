def choiceN[A](a: Par[Int])(choices: List[Par[A]]): Par[A] = 
  es => choices(a(es).get)(es)

def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
  choiceN(map(a)(b => if (b) 1 else 0))(List(ifTrue, ifFalse))