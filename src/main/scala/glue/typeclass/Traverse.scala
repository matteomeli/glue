package glue.typeclass

import glue.data.{Const, Identity}
import glue.data.Identity.implicits._

import Monoid.implicits._

trait Traverse[F[_]] {
  val functor: Functor[F]
  val foldable: Foldable[F]

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]// = sequence(functor.map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  // Traversing with the Identity applicative is equivalent to Functor.map
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Applicative[Identity].unit(f(a))).run

  // Traversing with Const applicative is quivalent to Foldable.foldMap
  def foldMap[A, M](fa: F[A])(f: A => M)(implicit monoid: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](fa)(a => Const(f(a))).run
}

object Traverse extends TraverseFunctions {
  def apply[F[_]](implicit traverse: Traverse[F]): Traverse[F] = traverse

  object syntax extends TraverseSyntax
}

trait TraverseFunctions {

}

trait TraverseSyntax {

}

trait TraverseLaws {

}
