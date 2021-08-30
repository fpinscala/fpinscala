def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = fork {
  val pars: List[Par[List[A]]] =
    l.map(asyncF(a => if f(a) then List(a) else List()))
  sequence(pars).map(_.flatten) // convenience method on `List` for concatenating a list of lists
}
