package glue.std

import glue.typeclass.{Applicative, Functor}

object either extends EitherFunctions with EitherSyntax with EitherImplicits

trait EitherFunctions {}

trait EitherSyntax {}

trait EitherImplicits {
  implicit def eitherIsApplicative[L]: Applicative[({type f[x] = Either[L, x]})#f] =
    new Applicative[({type f[x] = Either[L, x]})#f] {
      val functor: Functor[({type f[x] = Either[L, x]})#f] = Functor[({type f[x] = Either[L, x]})#f]
      def unit[A](a: => A): Either[L, A] = Right(a)
      def apply[A, B](f: Either[L, A => B])(e: Either[L, A]): Either[L, B] = e flatMap { a => f.map(_(a)) }
    }

  private implicit def eitherIsFunctor[L]: Functor[({type f[x] = Either[L, x]})#f] =
    new Functor[({type f[x] = Either[L, x]})#f] {
      def map[A, B](e: Either[L, A])(f: A => B): Either[L, B] = e map f
    }
}
