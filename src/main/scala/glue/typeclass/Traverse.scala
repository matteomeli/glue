package glue
package typeclass

import glue.data.{Const, Identity, State}
import glue.data.Identity.implicits._

import Monoid.implicits._

import State._
import State.implicits._

import glue.NaturalTransformation

trait Traverse[F[_]] { self =>
  val functor: Functor[F]
  val foldable: Foldable[F]

  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]// = sequence(functor.map(fa)(f))
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)

  // Traversing with the Identity applicative is equivalent to Functor.map
  def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse(fa)(a => Monad[Identity].applicative.pure(f(a)))(Monad[Identity].applicative).run

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

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad[({type f[x] = State[S, x]})#f].applicative)

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    traverseS(fa) { a => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i) }.run(0)._2

  def toList[A](fa: F[A]): List[A] =
    traverseS(fa) { a => for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ()}.run(Nil)._1.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (S, A) => (S, B)): (S, F[B]) =
    traverseS(fa) { a => for {
      s1 <- get[S]
      (s2, b) = f(s1, a)
      _ <- set(s2)
    } yield b }.run(s)

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((as, _) => (as.tail, as.head))._2

  def zipWith[A, B, C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): (List[B], F[C]) =
    mapAccum(fa, toList(fb)) {
      case (Nil, a) => (Nil, f(a, None))
      case (b :: bs, a) => (bs, f(a, Some(b)))
    }

  def zipWithL[A, B, C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): F[C] =
    zipWith(fa, fb)(f)._2

  def zipWithR[A, B, C](fa: F[A], fb: F[B])(f: (Option[A], B) => C): F[C] =
    zipWith(fb, fa)((b, oa) => f(oa, b))._2

  def zipL[A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] = zipWithL(fa, fb)((_, _))
  def zipR[A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] = zipWithR(fa, fb)((_, _))

  def mapM[M[_]: Monad, A, B](fa: F[A])(f: A => M[B]): M[F[B]] =
    traverse(fa)(f)(Monad[M].applicative)

  // A synonym for traverse
  def forA[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = traverse(fa)(f)
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
  def traverseS[F[_]: Traverse, S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = Traverse[F].traverseS(fa)(f)
  def zipWithIndex[F[_]: Traverse, A](fa: F[A]): F[(A, Int)] = Traverse[F].zipWithIndex(fa)
  def toList[F[_]: Traverse, A](fa: F[A]): List[A] = Traverse[F].toList(fa)
  def mapAccum[F[_]: Traverse, S, A, B](fa: F[A], s: S)(f: (S, A) => (S, B)): (S, F[B]) = Traverse[F].mapAccum(fa, s)(f)
  def reverse[F[_]: Traverse, A](fa: F[A]): F[A] = Traverse[F].reverse(fa)
  def zipWith[F[_]: Traverse, A, B, C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): (List[B], F[C]) = Traverse[F].zipWith(fa, fb)(f)
  def zipWithL[F[_]: Traverse, A, B, C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): F[C] = Traverse[F].zipWithL(fa, fb)(f)
  def zipWithR[F[_]: Traverse, A, B, C](fa: F[A], fb: F[B])(f: (Option[A], B) => C): F[C] = Traverse[F].zipWithR(fa, fb)(f)
  def zipL[F[_]: Traverse, A, B](fa: F[A], fb: F[B]): F[(A, Option[B])] = Traverse[F].zipL(fa, fb)
  def zipR[F[_]: Traverse, A, B](fa: F[A], fb: F[B]): F[(Option[A], B)] = Traverse[F].zipR(fa, fb)
  def mapM[F[_]: Traverse, M[_]: Monad, A, B](fa: F[A])(f: A => M[B]): M[F[B]] = Traverse[F].mapM(fa)(f)
  def forA[F[_]: Traverse, G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = Traverse[F].forA(fa)(f)
}

trait TraverseSyntax {
  implicit class TraverseOps[F[_]: Traverse, A](self: F[A]) {
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = Traverse[F].traverse(self)(f)
    def sequence[G[_]: Applicative, B](implicit ev: A <:< G[B]): G[F[B]] = {
      ev.unused
      Traverse[F].sequence(self.asInstanceOf[F[G[B]]])
    }
    def map[B](f: A => B): F[B] = Traverse[F].map(self)(f)
    def foldMap[B](f: A => B)(implicit M: Monoid[B]): B = Traverse[F].foldMap(self)(f)
    def fuse[G[_], H[_], B](f: A => G[B], g: A => H[B])(implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = Traverse[F].fuse(self)(f, g)
    def traverseS[S, B](f: A => State[S, B]): State[S, F[B]] = Traverse[F].traverseS(self)(f)
    def zipWithIndex: F[(A, Int)] = Traverse[F].zipWithIndex(self)
    def toList: List[A] = Traverse[F].toList(self)
    def mapAccum[S, B](s: S)(f: (S, A) => (S, B)): (S, F[B]) = Traverse[F].mapAccum(self, s)(f)
    def reverse: F[A] = Traverse[F].reverse(self)
    def zipWith[B, C](fb: F[B])(f: (A, Option[B]) => C): (List[B], F[C]) = Traverse[F].zipWith(self, fb)(f)
    def zipWithL[B, C](fb: F[B])(f: (A, Option[B]) => C): F[C] = Traverse[F].zipWithL(self, fb)(f)
    def zipWithR[B, C](fb: F[B])(f: (Option[A], B) => C): F[C] = Traverse[F].zipWithR(self, fb)(f)
    def zipL[B](fb: F[B]): F[(A, Option[B])] = Traverse[F].zipL(self, fb)
    def zipR[B](fb: F[B]): F[(Option[A], B)] = Traverse[F].zipR(self, fb)
    def mapM[M[_]: Monad, B](f: A => M[B]): M[F[B]] = Traverse[F].mapM(self)(f)
    def forA[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = Traverse[F].forA(self)(f)
  }
}

trait TraverseLaws[F[_]] {
  implicit val traversable: Traverse[F]

  import Traverse._

  def identity[A](fa: F[A]): Boolean =
    traversable.traverse(fa)(Identity(_))(Monad[Identity].applicative) == Identity(fa)

  def composition[G[_], H[_], A, B, C](fa: F[A], f: A => G[B], g: B => H[C])(implicit G: Applicative[G], H: Applicative[H]): Boolean = {
    val left: G[H[F[C]]] = traversable.traverse[({type f[x] = G[H[x]]})#f, A, C](fa)(a => G.map(f(a))(g))(G compose H)
    val right: G[H[F[C]]] = G.map(traverse(fa)(f))(fb => traverse(fb)(g))
    left == right
  }

  def purity[G[_], A](fa: F[A])(implicit G: Applicative[G]): Boolean =
    traversable.traverse(fa)(G.pure(_)) == G.pure(fa)

  def naturality[G[_], H[_], A, B](t: NaturalTransformation[G, H])(fa: F[A], f: A => G[B])(implicit G: Applicative[G], H: Applicative[H]): Boolean = {
    t(traversable.traverse(fa)(f)) == traversable.traverse(fa)(a => t(f(a)))
  }
}

object TraverseLaws {
  def apply[F[_]](implicit F: Traverse[F]): TraverseLaws[F] =
    new TraverseLaws[F] { val traversable: Traverse[F] = F }
}
