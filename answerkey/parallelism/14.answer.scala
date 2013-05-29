/* 
This implementation is not safe for execution on bounded thread pools, and it also does not preserve timeouts. Can you see why? You may wish to try implementing a nonblocking version like was done for `fork`.  
*/
def join[A](a: Par[Par[A]]): Par[A] = 
  es => a(es).get.apply(es)

def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] = 
  flatMap(a)(a => a)

def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] = 
  join(map(p)(f))