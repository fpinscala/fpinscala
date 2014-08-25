// In `sequence`, the base case of the fold is a `unit` action that returns
// the empty list. At each step in the fold, we accumulate in `acc`
// and `f` is the current element in the list.
// `map2(f, acc)(_ :: _)` results in a value of type `Rand[List[A]]`
// We map over that to prepend (cons) the element onto the accumulated list.
//
// We are using `foldRight`. If we used `foldLeft` then the values in the
// resulting list would appear in reverse order. It would be arguably better
// to use `foldLeft` followed by `reverse`. What do you think?
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

// It's interesting that we never actually need to talk about the `RNG` value
// in `sequence`. This is a strong hint that we could make this function
// polymorphic in that type.

def _ints(count: Int): Rand[List[Int]] =
  sequence(List.fill(count)(int))
