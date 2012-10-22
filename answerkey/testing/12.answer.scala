def &&(p: Prop) = Prop { 
  (n,rng) => run(n,rng) match {
    case Right((a,n)) => p.run(n,rng).right.map { case (s,m) => (s,n+m) }
    case l => l
  }
}
def ||(p: Prop) = Prop { 
  (n,rng) => run(n,rng) match {
    case Left(msg) => p.tag(msg).run(n,rng)
    case r => r 
  }
}
/* This is rather simplistic - in the event of failure, we simply prepend
 * the given message on a newline in front of the existing message. 
 */
def tag(msg: String) = Prop {
  (n,rng) => run(n,rng) match {
    case Left(e) => Left(msg + "\n" + e)
  }
}