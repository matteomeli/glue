package glue.typeclass

import glue.data.{Const, Identity}
import glue.data.Identity.implicits._

import Monoid.implicits._

import glue.NaturalTransformation

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
  def traverse[F[_]: Traverse, G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = Traverse[F].traverse(fa)(f)
  def sequence[F[_]: Traverse, G[_]: Applicative, A, B](fga: F[G[A]]): G[F[A]] = Traverse[F].sequence(fga)
  def map[F[_]: Traverse, A, B](fa: F[A])(f: A => B): F[B] = Traverse[F].map(fa)(f)
  def foldMap[F[_]: Traverse, A, B](fa: F[A])(f: A => B)(implicit M: Monoid[B]): B = Traverse[F].foldMap(fa)(f)
  def fuse[F[_]: Traverse, G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    Traverse[F].fuse(fa)(f, g)
}

trait TraverseSyntax {
  implicit class TraverseOps[F[_]: Traverse, A](self: F[A]) {
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = Traverse[F].traverse(self)(f)
    def sequence[G[_]: Applicative, B](implicit ev: A <:< G[B]): G[F[B]] = {
      import glue.data.Identity.syntax._
      ev.unused
      Traverse[F].sequence(self.asInstanceOf[F[G[B]]])
    }
    def map[B](f: A => B): F[B] = Traverse[F].map(self)(f)
    def foldMap[B](f: A => B)(implicit M: Monoid[B]): B = Traverse[F].foldMap(self)(f)
    def fuse[G[_], H[_], B](f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = Traverse[F].fuse(self)(f, g)
  }
}

trait TraverseLaws[F[_]] {
  implicit val traversable: Traverse[F]

  import Traverse._

  def identity[A](fa: F[A]): Boolean =
    traverse(fa)(Identity(_)) == Identity(fa)

  def composition[G[_], H[_], A, B, C](fa: F[A], f: A => G[B], g: B => H[C])(implicit G: Applicative[G], H: Applicative[H]): Boolean = {
    val left: G[H[F[C]]] = traversable.traverse[({type f[x] = G[H[x]]})#f, A, C](fa)(a => G.map(f(a))(g))(G compose H)
    val right: G[H[F[C]]] = G.map(traverse(fa)(f))(fb => traverse(fb)(g))
    left == right
  }

  def purity[G[_], A](fa: F[A])(implicit G: Applicative[G]): Boolean =
    traversable.traverse(fa)(G.unit(_)) == G.unit(fa)

  def naturality[G[_], H[_], A, B](fa: F[A], f: A => G[B], t: NaturalTransformation[G, H])(implicit G: Applicative[G], H: Applicative[H]): Boolean = {
    t(traversable.traverse(fa)(f)) == traversable.traverse(fa)(a => t(f(a)))
  }
}

object TraverseLaws {
  def apply[F[_]](implicit F: Traverse[F]): TraverseLaws[F] =
    new TraverseLaws[F] { val traversable: Traverse[F] = F }
}
