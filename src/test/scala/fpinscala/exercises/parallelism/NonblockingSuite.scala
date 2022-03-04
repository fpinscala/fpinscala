package fpinscala.exercises.parallelism

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Gen.`**`
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.parallelism.Nonblocking.*

import java.util.concurrent.*

class NonblockingSuite extends PropSuite:
  private val es = Executors.newFixedThreadPool(4)
  private val genParBoolean: Gen[Par[Boolean]] = Gen.boolean.map(Par.unit)
  private val genParInt: Gen[Par[Int]] = genShortNumber.map(Par.unit)
  private val genParString: Gen[Par[String]] = genString.map(Par.unit)
  private val genParParString: Gen[Par[Par[String]]] = genString.map(str => Par.unit(Par.unit(str)))
  private val genListOfParString: Gen[List[Par[String]]] =
    genNonEmptyList[Par[String]](genParString)
  private val genMap: Gen[Map[Int, Par[String]]] =
    Gen.listOfN(20, genParString).map(list => list.toIndexedSeq.indices.map(i => i -> list(i)).toMap)

  test("Nonblocking.choice")(genParInt ** genParInt ** genParBoolean) { case t ** f ** p =>
    checkChoice(t, f, p)(Par.choice[Int](p)(t, f))
  }

  test("Nonblocking.choiceN")(genParInt ** genListOfParString) { case p ** ps =>
    checkChoiceN(p, ps)(Par.choiceN[String](p)(ps))
  }

  test("Nonblocking.choiceViaChoiceN")(genParInt ** genParInt ** genParBoolean) { case t ** f ** p =>
    checkChoice(t, f, p)(Par.choiceViaChoiceN[Int](p)(t, f))
  }

  test("Nonblocking.choiceMap")(genParInt ** genMap) { case p ** ps =>
    val pc = Par.choiceMap[Int, String](p)(ps)
    val actual: String = pc.run(es)
    val key = p.run(es)
    val expected: String = ps(key).run(es)
    assertEquals(actual, expected)
  }

  test("Nonblocking.chooser")(genParInt ** genMap) { case p ** ps =>
    checkFlatMap(p, ps)(Par.chooser[Int, String](p)(ps))
  }

  test("Nonblocking.choiceViaFlatMap")(genParInt ** genParInt ** genParBoolean) { case t ** f ** p =>
    checkChoice(t, f, p)(Par.choiceViaFlatMap[Int](p)(t, f))
  }

  test("Nonblocking.choiceNViaFlatMap")(genParInt ** genListOfParString) { case p ** ps =>
    checkChoiceN(p, ps)(Par.choiceNViaFlatMap[String](p)(ps))
  }

  test("Nonblocking.join")(genParParString) { p =>
    val pc = Par.join[String](p)
    assertEquals(pc.run(es), p.run(es).run(es))
  }

  test("Nonblocking.joinViaFlatMap")(genParParString) { p =>
    val pc = Par.joinViaFlatMap[String](p)
    assertEquals(pc.run(es), p.run(es).run(es))
  }

  test("Nonblocking.flatMapViaJoin")(genParInt ** genMap) { case p ** ps =>
    checkFlatMap(p, ps)(Par.flatMapViaJoin[Int, String](p)(ps))
  }

  private def checkChoice(t: Par[Int], f: Par[Int], p: Par[Boolean])(choice: Par[Int]) =
    val actual = choice.run(es)
    val expected = if p.run(es) then t.run(es) else f.run(es)
    assertEquals(actual, expected)

  private def checkChoiceN(p: Par[Int], ps: List[Par[String]])(choice: Par[String]) =
    val actual = choice.run(es)
    val i = p.run(es) % ps.length
    val expected = ps(i).run(es)
    assertEquals(actual, expected)

  private def checkFlatMap(p: Par[Int], ps: Map[Int, Par[String]])(choice: Par[String]) =
    val actual: String = choice.run(es)
    val key = p.run(es)
    val expected: String = ps(key).run(es)
    assertEquals(actual, expected)
