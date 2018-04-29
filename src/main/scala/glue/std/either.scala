package glue.std

import glue.typeclass.{Applicative, Functor}

object either extends EitherFunctions with EitherSyntax with EitherImplicits

trait EitherFunctions {}

trait EitherSyntax {}

trait EitherImplicits {
  implicit def eitherIsFunctor[L]: Functor[({type λ[α] = Either[L, α]})#λ] =
    new Functor[({type λ[α] = Either[L, α]})#λ] {
      def map[A, B](e: Either[L, A])(f: A => B): Either[L, B] = e map f
    }

  implicit def eitherIsApplicative[L]: Applicative[({type λ[α] = Either[L, α]})#λ] =
    new Applicative[({type λ[α] = Either[L, α]})#λ] {
      val functor: Functor[({type λ[α] = Either[L, α]})#λ] = Functor[({type λ[α] = Either[L, α]})#λ]
      def unit[A](a: => A): Either[L, A] = Right(a)
      def apply[A, B](f: Either[L, A => B])(e: Either[L, A]): Either[L, B] = e flatMap { a => f.map(_(a)) }
    }
}
