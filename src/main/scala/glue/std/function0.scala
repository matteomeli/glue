package glue
package std

import typeclass.Monad

object function0 extends Function0Implicits

trait Function0Implicits {
  private implicit def function0IsFunctor: Functor[Function0] = new Functor[Function0] {
    def map[A, B](fa: () => A)(f: A => B): () => B = () => f(fa())
  }

  private implicit def function0IsApplicative: Applicative[Function0] = new Applicative[Function0] {
    val functor: Functor[Function0] = Functor[Function0]
    def pure[A](a: => A): () => A = () => a
    def apply[A, B](f: () => A => B)(fa: () => A): () => B = () => f()(fa())
  }

  // Not stack safe as flatMap is not stack safe
  implicit def function0IsMonad: Monad[Function0] = new Monad[Function0] {
    val applicative: Applicative[Function0] = Applicative[Function0]
    def flatMap[A, B](fa: () => A)(f: A => (() => B)): () => B = f(fa())
  }
}
