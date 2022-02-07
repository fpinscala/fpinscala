package fpinscala.state

import fpinscala.state.State.{get, modify, sequence}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def main(args: Array[String]): Unit = {
    println("Candy.main()")
    val m = Machine(locked = true, 10, 10)
    // simulateMachine()
  }

  def update = (i: Input) => (s: Machine) =>
  (i, s) match {
    case (_, Machine(_, 0, _)) => s
    case (Coin, Machine(false, _, _)) => s
    case (Turn, Machine(true, _, _)) => s
    case (Coin, Machine(true, candy, coin)) =>
      Machine(false, candy, coin + 1)
    case (Turn, Machine(false, candy, coin)) =>
      Machine(true, candy - 1, coin)
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}
