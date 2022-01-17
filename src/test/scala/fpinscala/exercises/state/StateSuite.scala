package fpinscala.exercises.state

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.state.State
import fpinscala.exercises.state.State.*

class StateSuite extends PropSuite:
  // a - the head element, next state - the tail of the list
  private val stateA: State[List[String], Option[String]] =
    State {
      case Nil          => (None, Nil)
      case head :: tail => (Some(head), tail)
    }

  // b - the length of the list, next state - the tail of the list
  private val stateB: State[List[String], Int] =
    State {
      case Nil          => (0, Nil)
      case head :: tail => (tail.length + 1, tail)
    }

  /*
  test("State.unit")(genString) { str =>
    val (a, s) = unit[Int, String](str).run(0)
    assertEquals(a, str)
    assertEquals(s, 0)
  }
  */

  test("State.map")(genStringList) { list =>
    val (b, s) = stateA.map(length).run(list)
    val expectedB = length(list.headOption)
    assertEquals(b, expectedB)
    assertEquals(s, list.drop(1))
  }

  test("State.map2")(genStringList) { list =>
    val (c, s) = stateA.map2(stateB)(printResult).run(list)
    // a - the result of the initial state, b - the result of the next state
    val expectedC = printResult(list.headOption, list.drop(1).length)
    // s - the result of passing through two states (after a and b)
    assertEquals(c, expectedC)
    assertEquals(s, list.drop(2))
  }

  /*
  test("State.flatMap")(genStringList) { list =>
    val (b, s) = stateA.flatMap(a => unit(length(a))).run(list)
    val expectedB = length(list.headOption)
    assertEquals(b, expectedB)
    assertEquals(s, list.drop(1))
  }

  test("State.sequence")(genStringList) { list =>
    val half = list.length / 2
    val listOfStates = (0 until half).map(_ => stateA).toList
    val (firstHalfElements, restElements) = sequence(listOfStates).run(list)
    val (first, rest) = list.splitAt(half)
    assertEquals(firstHalfElements, first.map(Some(_)))
    assertEquals(restElements, rest)
  }
  */

  private def length(maybeHead: Option[String]): Int =
    maybeHead.getOrElse("").length

  private def printResult(maybeHead: Option[String], length: Int): String =
    s"The head element is '$maybeHead', the length is $length"
