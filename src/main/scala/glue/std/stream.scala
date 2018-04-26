package glue.std

import glue.typeclass.{Foldable, Functor, Monoid}

object stream extends StreamFunctions with StreamSyntax with StreamInstances

trait StreamFunctions {}

trait StreamSyntax {}

trait StreamInstances {
  implicit val streamIsFoldable: Foldable[Stream] = new Foldable[Stream] {
    def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldMap[A, B](as: Stream[A])(f: A => B)(implicit M: Monoid[B]): B =
      as.foldLeft(M.unit)((b, a) => M.combine(b, f(a)))
  }

  implicit val streamIsFunctor: Functor[Stream] = new Functor[Stream] {
    def map[A, B](as: Stream[A])(f: A => B): Stream[B] = as map f
  }
}