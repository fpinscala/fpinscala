def &&(p: Prop) = Prop {
  (n,rng) => run(max,n,rng) orElse p.run(max,n,rng)
}

def ||(p: Prop) = Prop {
  (n,rng) => run(max,n,rng) flatMap {
    // In case of failure, run the other prop.
    case (msg, _) => p.tag(msg).run(max,n,rng)
  }
}

/* This is rather simplistic - in the event of failure, we simply prepend
 * the given message on a newline in front of the existing message.
 */
def tag(msg: String) = Prop {
  (n,rng) => run(max,n,rng) map {
    case (e, c) => (msg + "\n" + e, c)
  }
}