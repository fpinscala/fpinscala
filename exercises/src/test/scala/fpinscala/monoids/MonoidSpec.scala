package fpinscala.monoids

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks
import fpinscala.testing.{Gen => FPGen}
import java.util.concurrent.Executors

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class MonoidSpec extends FlatSpec with PropertyChecks {

  import fpinscala.testing.Prop
  import Prop._
  def runProp(prop: Prop, testCases: TestCases): Result = {
    import fpinscala.state.RNG.Simple
    prop.run(testCases, Simple(0))
  }

  def testMonoidLaws(testCases: TestCases): Result = {
    def intGen(max: Int) = FPGen.choose(0, max)
    def listGen[A](gen: FPGen[A]) = gen.listOfN(intGen(10))
    val stringGen = intGen(10) flatMap(FPGen.stringN)
    def optionGen[A](gen: FPGen[A]): FPGen[Option[A]] =
      for {
        b <- FPGen.boolean
        a <- gen
      } yield if (b) Some(a) else None

    import Monoid._
    val monoidProps =
      monoidLaws(stringMonoid, stringGen) &&
      monoidLaws(listMonoid[Int], listGen(intGen(100))) &&
      monoidLaws(listMonoid[String], listGen(stringGen)) &&
      monoidLaws(intAddition, intGen(100)) &&
      monoidLaws(intMultiplication, intGen(100)) &&
      monoidLaws(booleanOr, FPGen.boolean) &&
      monoidLaws(booleanAnd, FPGen.boolean) &&
      monoidLaws(optionMonoid[Int], optionGen(intGen(10)))

    runProp(monoidProps, testCases)
  }

  private def checkMonoidLaws[A](m: Monoid[A], gen: Gen[A],
      isEqual: (A,A) => Boolean = (_:A) == (_:A)) =
  {
    def assertEquals(a1: A, a2: A) = assert(isEqual(a1, a2), s"$a1 == $a2")
    import m._
    forAll(gen label "x", gen label "y", gen label "z") { (x: A, y: A, z: A) =>
      assertEquals(op(op(x, y), z), op(x, op(y, z)))
    }
    forAll(gen label "x") { x: A =>
      assertEquals(op(x, zero), x)
      assertEquals(op(zero, x), x)
    }
  }

  behavior of "10.1.1 intAddition"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.intAddition, Arbitrary.arbInt.arbitrary)
  }

  behavior of "10.1.2 intMultiplication"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.intMultiplication, Arbitrary.arbInt.arbitrary)
  }

  behavior of "10.1.3 booleanOr"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.booleanOr, Arbitrary.arbBool.arbitrary)
  }

  behavior of "10.1.4 booleanAnd"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.booleanAnd, Arbitrary.arbBool.arbitrary)
  }

  behavior of "10.2 optionMonoid"
  it should "obey the monoid laws" in {
    checkMonoidLaws(Monoid.optionMonoid[Int], Arbitrary.arbOption[Int].arbitrary)
    checkMonoidLaws(Monoid.optionMonoid[Boolean], Arbitrary.arbOption[Boolean].arbitrary)
    checkMonoidLaws(Monoid.optionMonoid[String], Arbitrary.arbOption[String].arbitrary)
  }

  private def isEqual[A,B](f: A => B, g: A => B)(implicit arb: Arbitrary[A]): Boolean = {
    forAll("a") { a: A => assert(f(a) == g(a)) }
    true
  }

  behavior of "10.3 endoMonoid"
  it should "obey the monoid laws" in {
    val booleanFunctionGen =
      Gen.oneOf[Boolean => Boolean]({x: Boolean => !x}, identity[Boolean] _)
    checkMonoidLaws(Monoid.endoMonoid[Boolean], booleanFunctionGen, isEqual[Boolean,Boolean])
  }

  behavior of "10.4 monoidLaws"
  it should "check all written monoids so far" in {
    import fpinscala.testing.Prop.Passed
    assert(testMonoidLaws(10) == Passed)
  }

  behavior of "10.5 foldMap"
  it should "work" in {
    forAll("ints") { ints: List[Int] =>
      val intsAsStrings = ints map(_.toString)
      assert(Monoid.foldMap(intsAsStrings, Monoid.intAddition)(_.toInt) == ints.sum)
    }
  }

  behavior of "10.6.1 foldRight"
  it should "work" in {
    val plus = (_:Int) + (_:Int)
    forAll("ints") { ints: List[Int] =>
      assert(Monoid.foldRight(ints)(0)(plus) == ints.sum)
    }
  }

  behavior of "10.6.2 foldLeft"
  it should "work" in {
    val plus = (_:Int) + (_:Int)
    forAll("ints") { ints: List[Int] =>
      assert(Monoid.foldLeft(ints)(0)(plus) == ints.sum)
    }
  }

  behavior of "10.7 foldMapV"
  it should "work" in {
    forAll("ints") { ints: List[Int] =>
      val intsAsStrings = ints.map(_.toString).toIndexedSeq
      assert(Monoid.foldMapV(intsAsStrings, Monoid.intAddition)(_.toInt) == ints.sum)
    }
  }

  behavior of "10.8 parFoldMap"
  it should "work" in {
    import fpinscala.parallelism.Nonblocking.Par
    val es = Executors.newFixedThreadPool(4)
    forAll("ints") { ints: List[Int] =>
      val intsAsStrings = ints.map(_.toString).toIndexedSeq
      val parSum = Monoid.parFoldMap(intsAsStrings, Monoid.intAddition)(_.toInt)
      assert(Par.run(es)(parSum) == ints.sum)
    }
  }

  behavior of "10.9 ordered"
  it should "work" in {
    assert(Monoid.ordered(IndexedSeq()))
    assert(Monoid.ordered(IndexedSeq(1)))
    assert(Monoid.ordered(IndexedSeq(-2, 0, 1, 3, 5)))
    assert(Monoid.ordered(IndexedSeq(-2, 0, 3, 1, 6)) == false)
    forAll("ints") {ints: Seq[Int] =>
      assert(Monoid.ordered(ints.toIndexedSeq) == (ints == ints.sorted))
    }
  }

  behavior of "10.10 wcMonoid"
  it should "obey the monoid laws" in {
    import fpinscala.testing.Gen
    import fpinscala.testing.Prop.Passed
    def intGen(max: Int) = Gen.choose(0, max)
    def listGen[A](gen: Gen[A]) = gen.listOfN(intGen(10))
    val stringGen = intGen(10) flatMap(Gen.stringN)
    val stubGen = stringGen map(Monoid.Stub(_))
    val partGen = for {
      lStub <- stringGen
      words <- intGen(10)
      rStub <- stringGen
    } yield Monoid.Part(lStub, words, rStub)
    val wcGen: Gen[Monoid.WC] = Gen.union(stubGen, partGen)
    val laws = Monoid.monoidLaws(Monoid.wcMonoid, wcGen)
    assert(runProp(laws, 10) == Passed)
  }

  behavior of "10.11 countWords"
  it should "work" in {
    val strGen: Gen[String] = {
      val whitespaceCharGen = Gen.oneOf(9.toChar, 10.toChar, 32.toChar)
      val nonWhitespaceCharGen = Gen.choose(33.toChar, 127.toChar)
      val charGen = Gen.frequency((1, whitespaceCharGen), (9, nonWhitespaceCharGen))
      Gen.listOf(charGen).map(_.mkString)
    }
    def wordCount(s: String) = {
      val s1 = s.trim
      if (s1 == "") 0 else s1.split("""\s+""").size
    }
    forAll(strGen label "s") { s: String =>
      assert(Monoid.countWords(s) == wordCount(s))
    }
  }

  val plus = (_:Int) + (_:Int)
  private def testFoldable[F[_]](foldable: Foldable[F], f: List[Int] => F[Int]) = {
    forAll("ints") { ints: List[Int] =>
      val intsF = f(ints)
      val sum = ints.sum
      assert(foldable.foldRight(intsF)(0)(plus) == sum)
      assert(foldable.foldLeft(intsF)(0)(plus) == sum)
      assert(foldable.foldMap(intsF)(_.toString)(Monoid.stringMonoid) ==
        ints.map(_.toString).fold("")(_ + _))
      assert(foldable.concatenate(intsF)(Monoid.intAddition) == sum)
//      assert(foldable.toList(intsF) == ints)
    }
  }

  behavior of "10.12.1 ListFoldable"
  it should "work" in {
    testFoldable(ListFoldable, identity)
  }

  behavior of "10.12.2 IndexedSeqFoldable"
  it should "work" in {
    testFoldable(IndexedSeqFoldable, _.toIndexedSeq)
  }

  behavior of "10.12.3 StreamFoldable"
  it should "work" in {
    testFoldable(StreamFoldable, _.toStream)
  }

  private implicit def arbTree[T](implicit ev: Arbitrary[T]): Arbitrary[Tree[T]] = {
    val maxDepth = 10 // to prevent StackOverflows

    def createLeaf: Gen[Tree[T]] = arbitrary[T] map (Leaf(_))
    def createBranch(depth: Int): Gen[Tree[T]] = {
      for {
        lIsLeaf <- arbitrary[Boolean]
        rIsLeaf <- arbitrary[Boolean]
        l <- createTree(lIsLeaf, depth)
        r <- createTree(rIsLeaf, depth)
      } yield Branch(l, r)
    }
    def createTree(isLeaf: Boolean, depth: Int): Gen[Tree[T]] =
      if (isLeaf || depth >= maxDepth) createLeaf else createBranch(depth + 1)

    Arbitrary {
      arbitrary[Boolean] flatMap { createTree(_, 0) }
    }
  }

  private def treeList[A](as: Tree[A]): List[A] = as match {
    case Leaf(a) => List(a)
    case Branch(l,r) => treeList(l) ::: treeList(r)
  }

  behavior of "10.13 TreeFoldable"
  it should "work" in {
  def treeSum(ints: Tree[Int]): Int = ints match {
    case Leaf(i) => i
    case Branch(l,r) => treeSum(l) + treeSum(r)
  }

  val foldable = TreeFoldable
  forAll("ints") { ints: Tree[Int] =>
      val sum = treeSum(ints)
      assert(foldable.foldRight(ints)(0)(plus) == sum)
      assert(foldable.foldLeft(ints)(0)(plus) == sum)
      assert(foldable.foldMap(ints)(_.toInt)(Monoid.intAddition) == sum)
      assert(foldable.concatenate(ints)(Monoid.intAddition) == sum)
//      assert(foldable.toList(ints) == treeList(ints))
    }
  }

  behavior of "10.14 OptionFoldable"
  it should "work" in {
    val foldable = OptionFoldable
    forAll("ints") { ints: Option[Int] =>
      val sum = ints.fold(0)(_ + 0)
      assert(foldable.foldRight(ints)(0)(plus) == sum)
      assert(foldable.foldLeft(ints)(0)(plus) == sum)
      assert(foldable.foldMap(ints)(_.toInt)(Monoid.intAddition) == sum)
      assert(foldable.concatenate(ints)(Monoid.intAddition) == sum)
//      assert(foldable.toList(ints) == ints.fold(List[Int]())(List(_)))
    }
  }

  behavior of "10.15 Foldable.toList"
  it should "work" in {
    forAll("ints") { ints: List[Int] =>
      assert(ListFoldable.toList(ints) == ints)
      assert(IndexedSeqFoldable.toList(ints.toIndexedSeq) == ints)
      assert(StreamFoldable.toList(ints.toStream) == ints)
    }
    forAll("ints") { ints: Tree[Int] =>
      assert(TreeFoldable.toList(ints) == treeList(ints))
    }
    forAll("ints") { ints: Option[Int] =>
      assert(OptionFoldable.toList(ints) == ints.fold(List[Int]())(List(_)))
    }
  }

  behavior of "10.16 productMonoid"
  it should "work" in {
    val pMonoid = Monoid.productMonoid(Monoid.intAddition, Monoid.intAddition)
    checkMonoidLaws[(Int,Int)](pMonoid, Arbitrary.arbTuple2[Int,Int].arbitrary)
  }

  behavior of "10.17 functionMonoid"
  it should "obey the monoid laws" in {
    val fMonoid = Monoid.functionMonoid[Int,String](Monoid.stringMonoid)
    val functionGen =
      Gen.oneOf[Int => String]({x: Int => x.toString}, {x: Int => x.toString + "x"})
    checkMonoidLaws[Int => String](fMonoid, functionGen, isEqual[Int,String] _)
  }

  behavior of "10.18 bag"
  it should "work" in {
    assert(Monoid.bag(Vector("a", "rose", "is", "a", "rose")) ==
      Map("a" -> 2, "rose" -> 2, "is" -> 1))

    val words = "Yesterday all my troubles seemed so far away".split(" ").toSeq
    val wordGen = Gen.oneOf(words)
    val sentenceGen = Gen.listOf(wordGen)
    forAll(sentenceGen label "as") { as: List[String] =>
      val wordBag = as.groupBy(identity).mapValues(_.size)
      assert(Monoid.bag(as.toIndexedSeq) == wordBag)
    }
  }
}
