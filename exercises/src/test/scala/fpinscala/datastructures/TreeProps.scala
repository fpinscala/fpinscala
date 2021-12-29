package fpinscala.datastructures

import fpinscala.datastructures.Tree.{Branch, Leaf}
import fpinscala.datastructures.TreeProps.property
import org.scalacheck.Gen.{lzy, oneOf}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

import scala.List as SList
import scala.language.adhocExtensions

object TreeProps extends Properties("fpinscala.datastructures.Tree"):
  private val genIntLeaf: Gen[Tree[Int]] = Gen.posNum[Int].map(n => Leaf(n))

  private lazy val genIntBranch: Gen[Tree[Int]] = for {
    left <- oneOf(genIntLeaf, genIntTree)
    right <- oneOf(genIntLeaf, genIntTree)
  } yield Branch(left, right)

  private val genIntTree: Gen[Tree[Int]] = oneOf(genIntLeaf, lzy(genIntBranch))

  private def listToScalaList[A](t: Tree[A]): SList[Option[A]] = t match
    case Leaf(v)      => SList(Some(v))
    case Branch(l, r) => (Option.empty[A] +: listToScalaList(l)) ++ listToScalaList(r)

  property("size") = forAll(genIntTree) { tree =>
    tree.size == listToScalaList(tree).length
  }

  property("depth") = forAll(genIntTree) { tree =>
    tree match
      case Leaf(_)      => tree.depth == 0
      case Branch(l, r) => tree.depth == 1 + l.depth.max(r.depth)
  }

  property("map") = forAll(genIntTree) { tree =>
    listToScalaList(tree.map(_.toString)) == listToScalaList(tree).map(_.map(_.toString))
  }

  property("fold") = forAll(genIntTree) { tree =>
    tree.fold(_.toString, _ + _) == listToScalaList(tree).flatMap(_.map(_.toString)).mkString
  }

  property("sizeViaFold") = forAll(genIntTree) { tree =>
    tree.sizeViaFold == listToScalaList(tree).length
  }

  property("depthViaFold") = forAll(genIntTree) { tree =>
    tree match
      case Leaf(_)      => tree.depthViaFold == 0
      case Branch(l, r) => tree.depthViaFold == 1 + l.depthViaFold.max(r.depthViaFold)
  }

  property("mapViaFold") = forAll(genIntTree) { tree =>
    listToScalaList(tree.mapViaFold(_.toString)) == listToScalaList(tree).map(_.map(_.toString))
  }

  property("size(tree)") = forAll(genIntTree) { tree =>
    Tree.size(tree) == listToScalaList(tree).length
  }

  property("firstPositive") = forAll(genIntTree) { tree =>
    tree.firstPositive == listToScalaList(tree).flatten.find(_ > 0)
  }

  property("maximum") = forAll(genIntTree) { tree =>
    tree.maximum == listToScalaList(tree).max.getOrElse(0)
  }

  property("maximumViaFold") = forAll(genIntTree) { tree =>
    tree.maximumViaFold == listToScalaList(tree).max.getOrElse(0)
  }
