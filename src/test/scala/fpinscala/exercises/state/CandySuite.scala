package fpinscala.exercises.state

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.state.Candy.*
import fpinscala.exercises.state.Input.*
import fpinscala.exercises.state.State.*
import fpinscala.exercises.state.{ Input, Machine, State }

class CandySuite extends PropSuite:
  private val genPosInt: Gen[Int]    = Gen.choose(1, 1000)
  private val genNonNegInt: Gen[Int] = Gen.choose(0, 1000)
  private val genInput: Gen[Input] =
    Gen.boolean.map(b => if b then Coin else Turn)
  private val genInputList: Gen[List[Input]] =
    genList(genInput)

  private val genNoCandiesMachine: Gen[Machine] =
    for
      locked <- Gen.boolean
      coins  <- genNonNegInt
    yield Machine(locked, 0, coins)

  private val genLockedMachine: Gen[Machine] =
    for
      candies <- genPosInt // A machine must have at least one candy
      coins <- genNonNegInt
    yield Machine(true, candies, coins)

  private val genUnlockedMachine: Gen[Machine] =
    for
      candies <- genPosInt // A machine must have at least one candy
      coins <- genNonNegInt
    yield Machine(false, candies, coins)

  private val genMachine: Gen[Machine] =
    for
      locked  <- Gen.boolean
      candies <- genNonNegInt
      coins   <- genNonNegInt
    yield Machine(locked, candies, coins)

  test("Candy: a machine that’s out of candy")(genInputList ** genNoCandiesMachine) { (inputs, machine) =>
    val ((coins, candies), machine1): ((Int, Int), Machine) = simulateMachine(inputs).run(machine)
    assertEquals(candies, 0)
    assertEquals(coins, machine.coins)
    assertEquals(machine1, machine) // A machine that’s out of candy ignores all inputs.
  }

  test("Candy: inserting a coin into a locked machine")(genLockedMachine) { machine =>
    val ((coins, candies), machine1): ((Int, Int), Machine) = simulateMachine(List(Coin)).run(machine)
    assertEquals(candies, machine.candies)
    assertEquals(coins, machine.coins + 1)                 // One more coin
    assertEquals(machine1, Machine(false, candies, coins)) // Unlock a machine
  }

  test("Candy: turning the knob on a locked machine")(genLockedMachine) { machine =>
    val ((coins, candies), machine1): ((Int, Int), Machine) = simulateMachine(List(Turn)).run(machine)
    assertEquals(candies, machine.candies)
    assertEquals(coins, machine.coins)
    assertEquals(machine1, machine) // Nothing changed
  }

  test("Candy: inserting a coin into an unlocked machine")(genUnlockedMachine) { machine =>
    val ((coins, candies), machine1): ((Int, Int), Machine) = simulateMachine(List(Coin)).run(machine)
    assertEquals(candies, machine.candies)
    assertEquals(coins, machine.coins)
    assertEquals(machine1, machine) // Nothing changed
  }

  test("Candy: turning the knob on an unlocked machine")(genUnlockedMachine) { machine =>
    val ((coins, candies), machine1): ((Int, Int), Machine) = simulateMachine(List(Turn)).run(machine)
    assertEquals(candies, machine.candies - 1) // The buyer has taken the candy
    assertEquals(coins, machine.coins)
    assertEquals(machine1, Machine(true, candies, coins)) // Lock a machine
  }

  test("Candy: spend some coins")(genLockedMachine ** genPosInt) { (machine, myCoins) =>
    val wantToSpendAllMyCoins = (0 until myCoins).flatMap(_ => List(Coin, Turn)).toList
    val ((coins, candies), machine1): ((Int, Int), Machine) =
      simulateMachine(wantToSpendAllMyCoins).run(machine)
    val spentCoins = math.min(machine.candies, myCoins)

    assertEquals(candies, machine.candies - spentCoins)
    assertEquals(coins, machine.coins + spentCoins)
    assertEquals(machine1, Machine(true, candies, coins))
  }

  test("Candy: empty inputs")(genMachine) { machine =>
    val ((coins, candies), machine1): ((Int, Int), Machine) = simulateMachine(List.empty[Input]).run(machine)
    assertEquals(candies, machine.candies)
    assertEquals(coins, machine.coins)
    assertEquals(machine1, machine) // Nothing changed
  }
