package fpinscala.datastructures

import org.specs2.mutable._

import Tree._ // implementations

object TreeSpec extends Specification {
  "Tree specification" title

  "Tree size" should { // ex 26
    "be 1 for a single leaf one" in {
      Tree.size(Leaf(1)) aka "size" mustEqual 1
    }

    "be 3 for a two leaf one" in {
      Tree.size(Branch(Leaf(1), Leaf(2))) aka "size" mustEqual 3
    }
  }

  "Maximum element" should { // ex 27
    "be 2 for single leaf 2" in {
      Tree.maximum(Leaf(2)) aka "max" mustEqual 2
    }

    "be 2 for two leafs" in {
      Tree.maximum(Branch(Leaf(1), Leaf(2))) aka "max" mustEqual 2
    }

    "be 5 for complex tree" in {
      Tree.maximum(Branch(
        Branch(Leaf(1), Leaf(3)),
        Branch(Leaf(4), Branch(Leaf(5), Leaf(2))))).
        aka("max") mustEqual 5

    }
  }

  "Tree depth" should { // ex 28
    "be 0 for a single leaf one" in {
      Tree.depth(Leaf(1)) aka "depth" mustEqual 0
    }

    "be 1 for a two leafs one" in {
      Tree.depth(Branch(Leaf(1), Leaf(2))) aka "depth" mustEqual 1
    }

    "be 2" in {
      Tree.depth(Branch(
        Branch(Leaf(1), Leaf(2)),
        Branch(Leaf(3), Leaf(4)))) aka "depth" mustEqual 2

    }
  }

  "Tree of integer" should { // ex 29
    "be mapped to character" >> {
      "for a single leaf one" in {
        Tree.map(Leaf(1)) { _.toChar }.
          aka("mapped tree") mustEqual Leaf(1.toChar)
      }

      "for a two leafs one" in {
        Tree.map(Branch(Leaf(1), Leaf(2))) { _.toChar }.
          aka("mapped tree") mustEqual Branch(Leaf(1.toChar), Leaf(2.toChar))
      }
    }
  }

  "Tree" should { // ex 30
    def br(a: Branch[Int]): Branch[Char] = a match {
      case Branch(Leaf(d), e @ Branch(_, _)) ⇒
        Branch(Leaf(d.toChar), br(e))
      case Branch(f @ Branch(_, _), Leaf(g)) ⇒
        Branch(br(f), Leaf(g.toChar))
      case Branch(h @ Branch(_, _), i @ Branch(_, _)) ⇒
        Branch(br(h), br(i))
      case Branch(Leaf(b), Leaf(c)) ⇒
        Branch(Leaf(b.toChar), Leaf(c.toChar))
    }

    def l(a: Leaf[Int]): Leaf[Char] = a match {
      case Leaf(i) ⇒ Leaf(i.toChar)
    }

    "be folded from leaf of integer to leaf of character" in {
      Tree.fold(Leaf(1))(l(_), br(_)).
        aka("folded tree") mustEqual Leaf(1.toChar)
    }

    "be folded from two leafs of integer to characters" in {
      Tree.fold(Branch(Leaf(1), Leaf(2)))(l(_), br(_)).
        aka("folded tree") mustEqual Branch(
          Leaf(1.toChar), Leaf(2.toChar))
    }
  }
}
