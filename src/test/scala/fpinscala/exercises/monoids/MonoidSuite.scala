package fpinscala.exercises.monoids

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Gen.`**`
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.monoids.Monoid.*
import fpinscala.exercises.monoids.Monoid.WC.*
import fpinscala.exercises.parallelism.Nonblocking.*

import java.util.concurrent.Executors

class MonoidSuite extends PropSuite:
  private val es = Executors.newFixedThreadPool(4)

  test("Monoid.stringMonoid")(genString ** genString ** genString) { case a ** b ** c =>
    assertMonoid(stringMonoid, a, b, c)
  }

  test("Monoid.listMonoid")(genIntList ** genIntList ** genIntList) { case a ** b ** c =>
    assertMonoid(listMonoid, a, b, c)
  }

  test("Monoid.intAddition")(Gen.int ** Gen.int ** Gen.int) { case a ** b ** c =>
    assertMonoid(intAddition, a, b, c)
  }

  test("Monoid.intMultiplication")(Gen.int ** Gen.int ** Gen.int) { case a ** b ** c =>
    assertMonoid(intMultiplication, a, b, c)
  }

  test("Monoid.booleanOr")(Gen.boolean ** Gen.boolean ** Gen.boolean) { case a ** b ** c =>
    assertMonoid(booleanOr, a, b, c)
  }

  test("Monoid.booleanAnd")(Gen.boolean ** Gen.boolean ** Gen.boolean) { case a ** b ** c =>
    assertMonoid(booleanAnd, a, b, c)
  }

  test("Monoid.optionMonoid")(genIntOption ** genIntOption ** genIntOption) { case a ** b ** c =>
    assertMonoid(optionMonoid[Int], a, b, c)
  }

  test("Monoid.dual")(genIntOption ** genIntOption ** genIntOption) { case a ** b ** c =>
    assertMonoid(dual(optionMonoid[Int]), a, b, c)
  }

  test("Monoid.endoMonoid")(Gen.int ** Gen.int ** Gen.int ** Gen.int) { case i0 ** i1 ** i2 ** i3 =>
    val m = endoMonoid[Int]
    val a: Int => Int = _ + i1
    val b: Int => Int = _ - i2
    val c: Int => Int = _ * i3

    assertEquals(m.combine(a, m.empty)(i0), a(i0), "identity")
    assertEquals(m.combine(m.empty, a)(i0), a(i0), "identity")
    assertEquals(m.combine(a, m.combine(b, c))(i0), m.combine(m.combine(a, b), c)(i0), "associativity")
  }

  /* GenSuite from chapter 8 must be passed
  test("Monoid.monoidLaws")(Gen.unit(())) { _ =>
    import fpinscala.exercises.testing.Gen as EGen
    import fpinscala.exercises.testing.Prop.Result.*

    val genInt = EGen.choose(Int.MinValue, Int.MaxValue)
    val genOption = genInt.map(i => if i % 2 == 0 then Some(i / 2) else None)

    assertEquals(monoidLaws(intAddition, genInt).check(), Passed)
    assertEquals(monoidLaws(intMultiplication, genInt).check(), Passed)
    assertEquals(monoidLaws(booleanOr, EGen.boolean).check(), Passed)
    assertEquals(monoidLaws(booleanAnd, EGen.boolean).check(), Passed)
    assertEquals(monoidLaws(optionMonoid[Int], genOption).check(), Passed)
  }
  */

  test("Monoid.combineAll")(genIntList ** genStringList ** genBooleanList) { case ilist ** slist ** blist =>
    assertEquals(combineAll(slist, stringMonoid), slist.mkString)
    assertEquals(combineAll(ilist, intAddition), ilist.sum)
    assertEquals(combineAll(ilist, intMultiplication), ilist.product)
    assertEquals(combineAll(blist, booleanOr), blist.exists(identity))
    assertEquals(combineAll(blist, booleanAnd), blist.forall(identity))
  }

  test("Monoid.foldMap")(genBooleanList) { list =>
    assertEquals(foldMap(list, intAddition)(trueCounter), trueCounter(list))
  }

  test("Monoid.foldRight")(genBooleanList) { list =>
    assertEquals(foldRight(list)(0)((b, acc) => trueCounter(b) + acc), trueCounter(list))
  }

  test("Monoid.foldLeft")(genBooleanList) { list =>
    assertEquals(foldLeft(list)(0)((acc, b) => trueCounter(b) + acc), trueCounter(list))
  }

  test("Monoid.foldMapV")(genBooleanList) { list =>
    assertEquals(foldMapV(list.toIndexedSeq, intAddition)(trueCounter), trueCounter(list))
  }

  test("Monoid.par")(genString ** genString ** genString) { case s1 ** s2 ** s3 =>
    val a = Par.unit(s1)
    val b = Par.unit(s2)
    val c = Par.unit(s3)
    val m = par(stringMonoid)

    assertEquals(m.combine(a, m.combine(b, c)).run(es), m.combine(m.combine(a, b), c).run(es), "associativity")
    assertEquals(m.combine(a, m.empty).run(es), a.run(es), "identity")
    assertEquals(m.combine(m.empty, a).run(es), a.run(es), "identity")
  }

  test("Monoid.parFoldMap")(genBooleanIndexedSeq) { seq =>
    assertEquals(parFoldMap(seq, intAddition)(trueCounter).run(es), trueCounter(seq))
  }

  test("Monoid.ordered")(genIntIndexedSeq) { ints =>
    assertEquals(ordered(ints), ints == ints.sorted)
  }

  private val genWC: Gen[WC] =
    def loop(): Gen[WC] =
      val genStub = genString.map(chars => Stub(chars))
      val genPart = for
        lStub <- genStringList
        words <- genShortNumber
        rStub <- genString
      yield Part(lStub.mkString(" "), words, rStub.mkString(" "))
      Gen.union(genStub, genPart)
    loop()

  test("Monoid.wcMonoid")(genWC ** genWC ** genWC) { case a ** b ** c =>
    assertMonoid(wcMonoid, a, b, c)
  }

  test("Monoid.count")(genStringList.map(_.mkString(" "))) { str =>
    val expected = if str.isEmpty then 0 else str.trim.split("\\s+").length
    assertEquals(count(str), expected)
  }

  given Monoid[Int] = intAddition
  given Monoid[String] = stringMonoid

  test("Monoid.productMonoid")(genString ** genString ** genString ** Gen.int ** Gen.int ** Gen.int) {
    case a1 ** b1 ** c1 ** a2 ** b2 ** c2 =>
      import Monoid.productMonoid
      assertMonoid(productMonoid[String, Int], (a1, a2), (b1, b2), (c1, c2))
  }

  import Monoid.given

  test("Monoid.mapMergeMonoid")(Gen.unit(())) { _ =>
    val M = mapMergeMonoid[String, Map[String, Int]]
    val m1 = Map("o1" -> Map("i1" -> 1, "i2" -> 2))
    val m2 = Map("o1" -> Map("i2" -> 3))
    val m3 = M.combine(m1, m2)
    assertEquals(m3, Map("o1" -> Map("i1" -> 1, "i2" -> 5)))
  }

  test("Monoid.functionMonoid")(Gen.int) { a =>
    val m: Monoid[Int => String] = functionMonoid[Int, String]
    val f: Int => String = i => if i % 2 == 0 then "even" else "odd"
    val g: Int => String = i => if i < 0 then "negative" else "positive"
    val h: Int => String = i => i.toString

    assertEquals(m.combine(f, m.empty)(a), f(a), "identity")
    assertEquals(m.combine(m.empty, f)(a), f(a), "identity")
    assertEquals(m.combine(f, m.combine(g, h))(a), m.combine(m.combine(f, g), h)(a), "associativity")
  }

  test("Monoid.bag")(Gen.unit(())) { _ =>
    assertEquals(bag(IndexedSeq.empty[String]), Map.empty[String, Int])
    assertEquals(bag(IndexedSeq("rose")), Map("rose" -> 1))
    assertEquals(bag(IndexedSeq("rose", "rose", "rose")), Map("rose" -> 3))
    assertEquals(bag(IndexedSeq("a", "rose", "is")), Map("a" -> 1, "rose" -> 1, "is" -> 1))
    assertEquals(bag(IndexedSeq("a", "rose", "is", "a", "rose")), Map("a" -> 2, "rose" -> 2, "is" -> 1))
  }

  private def trueCounter(b: Boolean): Int = if b then 1 else 0

  private def trueCounter(list: Seq[Boolean]): Int = list.count(identity)

  private def assertMonoid[A](m: Monoid[A], a: A, b: A, c: A): Unit =
    assertEquals(m.combine(a, m.combine(b, c)), m.combine(m.combine(a, b), c), "associativity")
    assertEquals(m.combine(a, m.empty), a, "identity")
    assertEquals(m.combine(m.empty, a), a, "identity")
