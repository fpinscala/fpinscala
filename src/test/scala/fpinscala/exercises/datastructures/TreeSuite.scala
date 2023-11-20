package fpinscala.exercises.datastructures

import fpinscala.answers.testing.exhaustive.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.datastructures.Tree.*
import fpinscala.exercises.datastructures.TreeSuite.genIntTree

import scala.List as SList

class TreeSuite extends PropSuite:
  test("Tree.size")(genIntTree): tree =>
    assertEquals(tree.size, toScalaList(tree).length)

  test("Tree.depth")(genIntTree):
    case tree@Leaf(_) => assertEquals(tree.depth, 0)
    case tree@Branch(l, r) => assertEquals(tree.depth, 1 + l.depth.max(r.depth))

  test("Tree.map")(genIntTree): tree =>
    assertEquals(toScalaList(tree.map(_.toString)), toScalaList(tree).map(_.map(_.toString)))

  test("Tree.fold")(genIntTree): tree =>
    assertEquals(tree.fold(_.toString, _ + _), toScalaList(tree).flatMap(_.map(_.toString)).mkString)

  test("Tree.sizeViaFold")(genIntTree): tree =>
    assertEquals(tree.sizeViaFold, toScalaList(tree).length)

  test("Tree.depthViaFold")(genIntTree):
    case tree@Leaf(_) => assertEquals(tree.depthViaFold, 0)
    case tree@Branch(l, r) => assertEquals(tree.depthViaFold, 1 + l.depthViaFold.max(r.depthViaFold))

  test("Tree.mapViaFold")(genIntTree): tree =>
    assertEquals(toScalaList(tree.mapViaFold(_.toString)), toScalaList(tree).map(_.map(_.toString)))

  test("size(tree)")(genIntTree): tree =>
    assertEquals(Tree.size(tree), toScalaList(tree).length)

  test("Tree.firstPositive")(genIntTree): tree =>
    val t = toScalaList(tree)
    assertEquals(tree.firstPositive, t.flatten.find(_ > 0).getOrElse(t.last.get))

  test("Tree.maximum")(genIntTree): tree =>
    assertEquals(tree.maximum, toScalaList(tree).max.getOrElse(0))

  test("Tree.maximumViaFold")(genIntTree): tree =>
    assertEquals(tree.maximumViaFold, toScalaList(tree).max.getOrElse(0))

  private def toScalaList[A](t: Tree[A]): SList[Option[A]] = t match
    case Leaf(v)      => SList(Some(v))
    case Branch(l, r) => (Option.empty[A] +: toScalaList(l)) ++ toScalaList(r)

object TreeSuite:
  val genIntTree: Gen[Tree[Int]] = genTree(Gen.int)

  private def genTree[A](g: Gen[A]): Gen[Tree[A]] =
    def loop(): Gen[Tree[A]] =
      Gen.boolean.flatMap:
        if _ then g.map(n => Leaf(n))
        else
          for
            left <- loop()
            right <- loop()
          yield Branch(left, right)
    loop()
