package glue.typeclass

import glue.data.{Const, Identity}
import glue.data.Identity.implicits._

import Monoid.implicits._

trait Traverse[F[_]] { self =>
  val functor: Functor[F]
  val foldable: Foldable[F]

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]// = sequence(functor.map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  // Traversing with the Identity applicative is equivalent to Functor.map
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Applicative[Identity].unit(f(a))).run

  // Traversing with Const applicative is quivalent to Foldable.foldMap
  def foldMap[A, M](fa: F[A])(f: A => M)(implicit M: Monoid[M]): M =
    traverse[({type f[x] = Const[M, x]})#f, A, Nothing](fa)(a => Const(f(a))).run

  // Fuse G and H applicative effects over the traversable F over a single traversal
  def fuse[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product H)

  // Compositon of two traversable F and G is traversable of type F[G[x]] for any type x
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      val foldable: Foldable[({type f[x] = F[G[x]]})#f] = self.foldable compose G.foldable
      val functor: Functor[({type f[x] = F[G[x]]})#f] = self.functor compose G.functor
      def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        self.traverse(fga) { ga => G.traverse(ga)(f) }
    }
}

object Traverse extends TraverseFunctions {
  def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  object syntax extends TraverseSyntax
}

trait TraverseFunctions {

}

trait TraverseSyntax {

}

trait TraverseLaws {

}
