package glue.std

import glue.typeclass.Functor

object either extends EitherFunctions with EitherSyntax with EitherInstances

trait EitherFunctions {}

trait EitherSyntax {}

trait EitherInstances {
  implicit def eitherIsFunctor[L]: Functor[({type λ[α] = Either[L, α]})#λ] =
    new Functor[({type λ[α] = Either[L, α]})#λ] {
      def map[A, B](e: Either[L, A])(f: A => B): Either[L, B] = e map f
    }
}
