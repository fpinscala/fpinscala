/* 
This implementation does not preserve timeouts, and you can probably see how it would be rather finnicky to do this correctly. This is an argument in favor of definining combinators like `parMap` in terms of simpler combinators.
*/
def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] =
  es => {
    val fs: List[Future[B]] = l map (a => asyncF(f)(a)(es)) 
    UnitFuture(fs.map(_.get))
  }