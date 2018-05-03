package glue.std

import glue.typeclass.{Applicative, Foldable, Functor, Monoid, Traverse}

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

  implicit val streamIsApplicative: Applicative[Stream] = new Applicative[Stream] {
    val functor: Functor[Stream] = Functor[Stream]
    def unit[A](a: => A): Stream[A] = Stream.continually(a)
    def apply[A, B](fs: Stream[A => B])(as: Stream[A]): Stream[B] =
      as zip fs map { case (a, f) => f(a) }
  }

  implicit val streamIsTraversable: Traverse[Stream] = new Traverse[Stream] {
    val foldable: Foldable[Stream] = Foldable[Stream]
    val functor: Functor[Stream] = Functor[Stream]
    def traverse[G[_], A, B](as: Stream[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[Stream[B]] =
      as.foldRight(applicative.unit(Stream[B]())) { (a, gl) =>
        applicative.map2(f(a), gl) { (b, l) => b #:: l }
      }
  }

  private implicit def streamIsFunctor: Functor[Stream] = new Functor[Stream] {
    def map[A, B](as: Stream[A])(f: A => B): Stream[B] = as map f
  }
}