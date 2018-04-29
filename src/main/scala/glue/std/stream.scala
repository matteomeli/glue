package glue.std

import glue.typeclass.{Applicative, Foldable, Functor, Monoid}

object stream extends StreamFunctions with StreamSyntax with StreamImplicits

trait StreamFunctions {}

trait StreamSyntax {}

trait StreamImplicits {
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

  implicit val streamIsApplicative: Applicative[Stream] = new Applicative[Stream] {
    val functor: Functor[Stream] = Functor[Stream]
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def apply[A, B](fs: Stream[A => B])(as: Stream[A]): Stream[B] =
      as zip fs map { case (a, f) => f(a) }
  }
}