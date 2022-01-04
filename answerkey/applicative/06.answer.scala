enum Validated[+E, +A]:
  case Valid(get: A) extends Validated[Nothing, A]
  case Invalid(error: E) extends Validated[E, Nothing]

object Validated:
  given validatedApplicative[E: Monoid]: Applicative[Validated[E, _]] with
    def unit[A](a: => A) = Valid(a)
    extension [A](fa: Validated[E, A])
      override def map2[B, C](fb: Validated[E, B])(f: (A, B) => C) =
        (fa, fb) match
          case (Valid(a), Valid(b)) => Valid(f(a, b))
          case (Invalid(e1), Invalid(e2)) =>
            Invalid(summon[Monoid[E]].combine(e1, e2))
          case (e @ Invalid(_), _) => e
          case (_, e @ Invalid(_)) => e
