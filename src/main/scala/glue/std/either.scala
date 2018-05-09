package glue
package std

import glue.typeclass.{Applicative, Foldable, Functor, Monad, Monoid, Traverse}

object either extends EitherImplicits

trait EitherImplicits {
  implicit def eitherIsFoldable[L]: Foldable[({type f[x] = Either[L, x]})#f] =
    new Foldable[({type f[x] = Either[L, x]})#f] {
      def foldLeft[A, B](e: Either[L, A])(z: B)(f: (B, A) => B): B = e match {
        case Left(_) => z
        case Right(a) => f(z, a)
      }
      def foldRight[A, B](e: Either[L, A])(z: B)(f: (A, B) => B): B = foldLeft(e)(z) { (b, a) => f(a, b) }
      def foldMap[A, B](e: Either[L, A])(f: A => B)(implicit M: Monoid[B]): B =
        foldLeft(e)(M.unit) { (b, a) => M.combine(b, f(a)) }
    }

  implicit def eitherIsMonad[L]: Monad[({type f[x] = Either[L, x]})#f] =
    new Monad[({type f[x] = Either[L, x]})#f] {
      val applicative: Applicative[({type f[x] = Either[L, x]})#f] = Applicative[({type f[x] = Either[L, x]})#f]
      def flatMap[A, B](e: Either[L, A])(f: A => Either[L, B]): Either[L, B] = e flatMap f
    }

  private implicit def eitherIsApplicative[L]: Applicative[({type f[x] = Either[L, x]})#f] =
    new Applicative[({type f[x] = Either[L, x]})#f] {
      val functor: Functor[({type f[x] = Either[L, x]})#f] = Functor[({type f[x] = Either[L, x]})#f]
      def unit[A](a: => A): Either[L, A] = Right(a)
      def apply[A, B](f: Either[L, A => B])(e: Either[L, A]): Either[L, B] = e flatMap { a => f.map(_(a)) }
    }

  private implicit def eitherIsFunctor[L]: Functor[({type f[x] = Either[L, x]})#f] =
    new Functor[({type f[x] = Either[L, x]})#f] {
      def map[A, B](e: Either[L, A])(f: A => B): Either[L, B] = e map f
    }

  implicit def eitherIsTraversable[L]: Traverse[({type f[x] = Either[L, x]})#f] =
    new Traverse[({type f[x] = Either[L, x]})#f] {
      val foldable: Foldable[({type f[x] = Either[L, x]})#f] = Foldable[({type f[x] = Either[L, x]})#f]
      val functor: Functor[({type f[x] = Either[L, x]})#f] = Functor[({type f[x] = Either[L, x]})#f]
      def traverse[G[_], A, B](e: Either[L, A])(f: A => G[B])(implicit G: Applicative[G]): G[Either[L, B]] =
        e fold (l => G.unit(Left(l)), a => G.map(f(a))(Right(_)))
    }
}
