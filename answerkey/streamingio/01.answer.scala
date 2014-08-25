def take[I](n: Int): Process[I,I] =
  if (n <= 0) Halt()
  else await(i => emit(i, take[I](n-1)))

def drop[I](n: Int): Process[I,I] =
  if (n <= 0) id
  else await(i => drop[I](n-1))

def takeWhile[I](f: I => Boolean): Process[I,I] =
  await(i =>
    if (f(i)) emit(i, takeWhile(f))
    else      Halt())

def dropWhile[I](f: I => Boolean): Process[I,I] =
  await(i =>
    if (f(i)) dropWhile(f)
    else      emit(i,id))
