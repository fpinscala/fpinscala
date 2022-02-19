package fpinscala.exercises.monoids

import fpinscala.answers.testing.exhaustive.*
import fpinscala.answers.testing.exhaustive.Gen.`**`
import fpinscala.answers.testing.exhaustive.Prop.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite
import fpinscala.exercises.datastructures.Tree
import fpinscala.exercises.datastructures.Tree.{Branch, Leaf}
import fpinscala.exercises.datastructures.TreeSuite.genIntTree
import fpinscala.exercises.monoids.Foldable.*
import fpinscala.exercises.monoids.Monoid.*

class FoldableSuite extends PropSuite:
  import Foldable.given
  given Monoid[Int] = intAddition
  given Monoid[String] = stringMonoid

  test("Foldable[List]")(genStringList) { list =>
    val expected = list.map(_.length).sum
    assertEquals(list.foldRight(0)((s, acc) => s.length + acc), expected)
    assertEquals(list.foldLeft(0)((acc, s) => s.length + acc), expected)
    assertEquals(list.foldMap[Int](_.length), expected)
    assertEquals(list.combineAll, list.mkString)
  }

  test("Foldable[IndexedSeq]")(genStringIndexedSeq) { seq =>
    val expected = seq.map(_.length).sum
    assertEquals(seq.foldRight(0)((s, acc) => s.length + acc), expected)
    assertEquals(seq.foldLeft(0)((acc, s) => s.length + acc), expected)
    assertEquals(seq.foldMap[Int](_.length), expected)
    assertEquals(seq.combineAll, seq.mkString)
  }

  test("Foldable[LazyList]")(genStringLazyList) { lazylist =>
    val expected = lazylist.map(_.length).sum
    assertEquals(lazylist.foldRight(0)((s, acc) => s.length + acc), expected)
    assertEquals(lazylist.foldLeft(0)((acc, s) => s.length + acc), expected)
    assertEquals(lazylist.foldMap[Int](_.length), expected)
    assertEquals(lazylist.combineAll, lazylist.mkString)
  }

  test("Foldable[Tree]")(genIntTree) { tree =>
    given Monoid[Int] = new:
      def combine(a1: Int, a2: Int): Int = a1 max a2
      val empty: Int = Int.MinValue

    val expected = tree.maximum
    assertEquals(tree.foldRight(Int.MinValue)(_ max _), expected)
    assertEquals(tree.foldLeft(Int.MinValue)(_ max _), expected)
    assertEquals(tree.foldMap[Int](identity), expected)
    assertEquals(tree.combineAll, expected)
  }

  test("Foldable[Option]")(genIntOption) { option =>
    val expected = option.getOrElse(0)
    assertEquals(option.foldRight(0)(_ + _), expected)
    assertEquals(option.foldLeft(0)(_ + _), expected)
    assertEquals(option.foldMap[Int](identity), expected)
    assertEquals(option.combineAll, expected)
  }

  test("Foldable[List].toList")(genStringList) { list =>
    import Foldable.given_Foldable_List.toList
    assertEquals(toList(list), list)
  }

  test("Foldable[IndexedSeq].toList")(genStringIndexedSeq) { seq =>
    import Foldable.given_Foldable_IndexedSeq.toList
    assertEquals(toList(seq), List.from(seq))
  }

  test("Foldable[LazyList].toList")(genStringLazyList) { lazylist =>
    import Foldable.given_Foldable_LazyList.toList
    assertEquals(toList(lazylist), List.from(lazylist))
  }

  test("Foldable[Tree].toList")(genIntTree) { tree =>
    import Foldable.given_Foldable_Tree.toList
    assertEquals(toList(tree), treeToList(tree))
  }

  test("Foldable[Option].toList")(genIntOption) { option =>
    import Foldable.given_Foldable_Option.toList
    assertEquals(toList(option), List.from(option))
  }

  private def treeToList[A](t: Tree[A]): List[A] = t match
    case Leaf(v)      => List(v)
    case Branch(l, r) => treeToList(l) ++ treeToList(r)
