def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
  es => {
    val ind = n.run(es).get // Full source files
    choices(ind).run(es)
  }

def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
  choiceN(cond.map(b => if b then 0 else 1))(List(t, f))
