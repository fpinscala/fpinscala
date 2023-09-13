  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = (a) match {
    case Return(a) => a
    case Suspend(r) => runTrampoline(r())
    case FlatMap(x,f) => x match {
      case Return(a) => runTrampoline { f(a) }
      case Suspend(r) => runTrampoline { r() flatMap f }
      case FlatMap(a0,g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    }
  }
