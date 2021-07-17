/*
Somewhat overkill, but to illustrate the feature we're using a _pattern guard_, to only match a `Cons` whose head
satisfies our predicate, `f`. The syntax is to add `if <cond>` after the pattern, before the `=>`, where `<cond>` can
use any of the variables introduced by the pattern.
*/
def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
  l match
    case Cons(h,t) if f(h) => dropWhile(t, f)
    case _ => l

