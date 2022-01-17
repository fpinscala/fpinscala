package fpinscala.answers.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State

trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join
    override def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))
    override def map2[B,C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  override def apply[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    ff.flatMap(f => fa.map(f))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  extension [A](ffa: F[F[A]])
    def join: F[A] = ffa.flatMap(identity)

object Monad:

  // Monad composition
  def composeM[G[_], H[_]](using G: Monad[G], H: Monad[H], T: Traverse[H]): Monad[[x] =>> G[H[x]]] = new:
    def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
    extension [A](gha: G[H[A]])
      override def flatMap[B](f: A => G[H[B]]): G[H[B]] =
        // Note: we must explicitly call `G.flatMap` and `G.map` or else we inadvertently get the extension methods from this composed instance
        G.flatMap(gha)(ha => G.map(T.traverse(ha)(f))(H.join))
