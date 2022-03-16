package fpinscala.exercises.applicative

import fpinscala.answers.testing.exhaustive.Gen.{`**`, int as genInt}
import fpinscala.exercises.applicative.ApplicativeSuite.*
import fpinscala.exercises.common.Common.*
import fpinscala.exercises.common.PropSuite

class MonadSuite extends PropSuite:
  opaque type Composed[A] = Either[String, Option[A]]

  private val composedMonad = Monad.composeM[Either[String, _], Option[_]]

  private val tm: TestedMonad[Composed] =
    new TestedMonad[Composed]:
      val monad: Monad[Composed] = composedMonad
      def pure[A]: A => Composed[A] = a => Right(Some(a))

  test("Monad.composeM")(genInt ** genString) { case n ** s =>
    assertMonad[Composed](tm, n, s)
  }
