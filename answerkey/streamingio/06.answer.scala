// this uses the `zip` function defined in exercise 7
def zipWithIndex: Process[I,(O,Int)] =
  this zip (count map (_ - 1))
