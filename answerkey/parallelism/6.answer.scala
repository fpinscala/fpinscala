def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
  val pars: List[Par[List[A]]] = 
    l map (asyncF((a: A) => if (f(a)) List(a) else List())) 
  map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
}