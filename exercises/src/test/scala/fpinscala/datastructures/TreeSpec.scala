package fpinscala.datastructures

import org.junit.runner.RunWith
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.PropertyChecks

import Tree._

@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class TreeSpec extends FlatSpec with PropertyChecks {

  private implicit def arbTree[T](implicit ev: Arbitrary[T]): Arbitrary[Tree[T]] = {
    val maxDepth = 10// to prevent StackOverflows

    def createLeaf: Gen[Tree[T]] = arbitrary[T] map (Leaf(_))
    def createBranch(depth: Int): Gen[Tree[T]] = {
      for {
        lIsLeaf <- arbitrary[Boolean]
        rIsLeaf <- arbitrary[Boolean]
        l <- createTree(lIsLeaf, depth)
        r <- createTree(rIsLeaf, depth)
//        l <- arbTree[T].arbitrary
//        r <- arbTree[T].arbitrary
      } yield Branch(l, r)
    }
    def createTree(isLeaf: Boolean, depth: Int): Gen[Tree[T]] =
      if (isLeaf || depth >= maxDepth) createLeaf else createBranch(depth + 1)

    Arbitrary {
      arbitrary[Boolean] flatMap { createTree(_, 0) }
    }
  }
//  println(s"tree=${arbTree[Int].arbitrary.sample.get}")
//  println(s"tree=${arbTree[Int].arbitrary.sample.get}")
//  println(s"tree=${arbTree[Int].arbitrary.sample.get}")

  val leaf1 = Leaf(1)
  val tree1 = Branch(leaf1, Leaf(2))
  val tree2 = Branch(tree1, Leaf(3))
  val tree3 = Branch(tree2, tree1)

  behavior of "3.25 size"

  it should "work" in {
    def testSize(t: Tree[_], expected: Int) = assertResult(expected)(size(t))

    val tests = Table(
      ("t", "size(t)"),
      (leaf1, 1),
      (tree1, 3),
      (tree2, 5),
      (tree3, 9))
    forAll(tests)(testSize)
  }

  it should "for all t: Tree[_] count the right size" in {
    forAll("t") { t: Tree[Int] =>
      def getSize = t.toString.split("Leaf|Branch").size - 1
      assertResult(getSize)(size(t))
    }
  }

  it should "for all t1,t2: Tree[_] ==> size(Branch(t1,t2)) == size(t1)+size(t2)+1" in {
    forAll("t1", "t2") { (t1: Tree[Int], t2: Tree[Int]) =>
      assertResult(size(t1)+size(t2)+1)(size(Branch(t1,t2)))
    }
  }

  behavior of "3.26 maximum"

  it should "work" in {
    def testMaximum(t: Tree[Int], expected: Int) = assertResult(expected)(maximum(t))

    val tests = Table(
      ("t", "maximum(t)"),
      (leaf1, 1),
      (tree1, 2),
      (tree2, 3),
      (tree3, 3))
    forAll(tests)(testMaximum)
  }

  it should "for all t1,t2: Tree[Int] ==> maximum(Branch(t1,t2)) == maximum(t1) max maximum(t2)" in {
    forAll("t1", "t2") { (t1: Tree[Int], t2: Tree[Int]) =>
      assertResult(maximum(t1) max maximum(t2))(maximum(Branch(t1,t2)))
    }
  }

  behavior of "3.27 depth"

  it should "work" in {
    def testDepth(t: Tree[_], expected: Int) = assertResult(expected)(depth(t))

    val tests = Table(
      ("t", "depth(t)"),
      (leaf1, 0),
      (tree1, 1),
      (tree2, 2),
      (tree3, 3))
    forAll(tests)(testDepth)
  }

  it should "for all t1,t2: Tree[Int] ==> depth(Branch(t1,t2)) == (depth(t1) max depth(t2)) + 1" in {
    forAll("t1", "t2") { (t1: Tree[Int], t2: Tree[Int]) =>
      assertResult((depth(t1) max depth(t2)) + 1)(depth(Branch(t1,t2)))
    }
  }

  behavior of "3.28 map"

  it should "work" in {
    def testMap(t: Tree[_], expected: Tree[String]) = assertResult(expected)(map(t)(_.toString + "x"))

    val tests = Table(
      ("t", "map(t)(...)"),
      (leaf1, Leaf("1x")),
      (tree1, Branch(Leaf("1x"), Leaf("2x"))),
      (tree2, Branch(Branch(Leaf("1x"), Leaf("2x")), Leaf("3x"))),
      (tree3, Branch(Branch(Branch(Leaf("1x"), Leaf("2x")), Leaf("3x")), Branch(Leaf("1x"), Leaf("2x")))))
    forAll(tests)(testMap)
  }

  it should "for all t: Tree[Int] ==> inverse-map(map(t)) == t" in {
    forAll("t") { t: Tree[Int] =>
      assertResult(t)(map(map(t)(_.toString))(_.toInt))
    }
  }

  behavior of "3.29.1 fold"

  it should "for all t: Tree[_] ==> fold(t)(Leaf(_))(Branch(_,_)) == t" in {
    forAll("t") { t: Tree[Int] =>
      assertResult(t)(fold[Int,Tree[Int]](t)(Leaf(_))(Branch(_,_)))
    }
  }

  behavior of "3.29.2 sizeViaFold"

  it should "for all t: Tree[_] ==> sizeViaFold(t) == size(t)" in {
    forAll("t") { t: Tree[Int] =>
      assertResult(size(t))(sizeViaFold(t))
    }
  }

  behavior of "3.29.3 maximumViaFold"

  it should "for all t: Tree[_] ==> maximumViaFold(t) == maximum(t)" in {
    forAll("t") { t: Tree[Int] =>
      assertResult(maximum(t))(maximumViaFold(t))
    }
  }

  behavior of "3.29.3 maximumViaFold"

  it should "for all t: Tree[_] ==> depthViaFold(t) == depth(t)" in {
    forAll("t") { t: Tree[Int] =>
      assertResult(depth(t))(depthViaFold(t))
    }
  }
}