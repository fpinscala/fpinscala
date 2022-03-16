package fpinscala.exercises.applicative

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Gen.`**`
import fpinscala.exercises.applicative.ApplicativeSuite.applicativeViaUnitAndMap2
import fpinscala.exercises.applicative.TraverseSuite.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.monads.Id

class TraverseSuite extends PropSuite:
  import Traverse.given

  test("Traverse.listTraverse")(genIntList) { intList =>
    assertEquals(intList.traverse(Id.apply), Id(intList))
    assertEquals(intList.map(Id.apply).sequence, Id(intList))
  }

  test("Traverse.optionTraverse")(genIntOption) { optInt =>
    assertEquals(optInt.traverse(Id.apply), Id(optInt))
    assertEquals(optInt.map(Id.apply).sequence, Id(optInt))
  }

  test("Traverse.treeTraverse")(genIntTree) { intTree =>
    assertEquals(intTree.traverse(Id.apply), Id(intTree))
    assertEquals(wrapTree(intTree).sequence, Id(intTree))
  }

  test("Traverse.mapTraverse")(genMap) { siMap =>
    assertEquals(siMap.traverse(Id.apply), Id(siMap))
    assertEquals(siMap.map { case (k, v) => k -> Id(v) }.sequence, Id(siMap))
  }

  test("Traverse.map")(genIntList) { intList =>
    import Traverse.listTraverse.map

    assertEquals(map(intList)(Id.apply), intList.map(Id.apply))
  }

  test("Traverse.reverse")(genIntList ** genIntList) { case x ** y =>
    import Traverse.listTraverse.{reverse, toList}

    assertEquals(reverse(x), x.reverse)
    assertEquals(toList(reverse(x)) ++ toList(reverse(y)), reverse(toList(y) ++ toList(x)))
  }

  test("Traverse.foldLeft")(genIntList) { intList =>
    import Traverse.listTraverse.foldLeft

    assertEquals(foldLeft(intList)(1)(_ * _), intList.product)
  }

  test("Traverse.fuse")(genIntList) { intList =>
    import Traverse.listTraverse.fuse

    val f: Int => Option[String] = i => if i % 2 == 0 then Some("even") else None
    val g: Int => Id[String] = i => if i % 2 == 0 then Id("even") else Id("odd")
    val expected: (Option[List[String]], Id[List[String]]) =
      intList.foldRight((Option(List.empty[String]), Id(List.empty[String]))) { case (i, (opt, id)) =>
        if i % 2 == 0 then (opt.map("even" :: _), id.map("even" :: _))
        else (None, id.map("odd" :: _))
      }

    assertEquals(fuse(intList)(f, g), expected)
  }

  private val optListTraverse = listTraverse.compose(using optionTraverse)
  import optListTraverse.{sequence, traverse}

  test("Traverse.compose")(genList(genIntOption)) { intOptList =>
    assertEquals(intOptList.traverse(Id.apply), Id(intOptList))
    assertEquals(intOptList.map(Id.apply).sequence, Id(intOptList))
  }

object TraverseSuite:
  private def wrapTree(tree: Tree[Int]): Tree[Id[Int]] =
    Tree(Id(tree.head), tree.tail.map(wrapTree))

  private val genIntTree: Gen[Tree[Int]] = genTree(Gen.int)

  private def genTree[A](g: Gen[A]): Gen[Tree[A]] =
    def loop(): Gen[Tree[A]] =
      Gen.boolean.flatMap {
        if _ then g.map(head => Tree(head, List.empty))
        else
          for
            head <- g
            tree <- loop()
          yield Tree(head, List(tree))
      }
    loop()
