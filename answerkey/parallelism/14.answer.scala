def join[A](ppa: Par[Par[A]]): Par[A] =
  es => ppa.run(es).get().run(es)

def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] =
  ppa.flatMap(identity)

extension [A](pa: Par[A]) def flatMapViaJoin[B](f: A => Par[B]): Par[B] =
  join(pa.map(f))
