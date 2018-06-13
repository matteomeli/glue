package glue
package free

import Free._

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Pure(_)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Chain(this, f)
  def apply[B](f: Free[F, A => B]): Free[F, B] = flatMap { a => f.map(_(a)) }

  def run(implicit ev: Free[F, A] =:= Trampoline[A]): A =
    runTrampoline(ev(this))

  def runM(implicit F: Monad[F]): F[A] = Free.runM(this)
}

case class Pure[F[_], A](a: A) extends Free[F, A]
case class Effect[F[_], A](e: F[A]) extends Free[F, A]
case class Chain[F[_], A0, A](v: Free[F, A0], f: A0 => Free[F, A]) extends Free[F, A]

object Free extends FreeFunctions {
  type Trampoline[A] = Free[Function0, A]

  @annotation.tailrec
  def runTrampoline[A](t: Trampoline[A]): A = t match {
    case Pure(a) => a
    case Effect(e) => e()
    case Chain(v, f) => v match {
      case Pure(a) => runTrampoline(f(a))
      case Effect(e) => runTrampoline(f(e()))
      case Chain(w, g) => runTrampoline(w flatMap (g andThen f))
    }
  }

  @annotation.tailrec
  def runM[F[_], A](fa: Free[F, A])(implicit F: Monad[F]): F[A] = fa match {
    case Pure(a) => F.pure(a)
    case Effect(e) => e
    case Chain(v, f) => v match {
      case Pure(a) => runM(f(a))
      case Effect(e) => ???
      case Chain(v, f) => ???
    }
  }

  def runFree[F[_], G[_], A](fa: Free[F, A])(k: NaturalTransformation[F, G])(implicit G: Monad[G]): G[A] = ???

  object implicits extends FreeImplicits
}

trait FreeFunctions {
  def map[F[_], A, B](v: Free[F, A])(f: A => B): Free[F, B] = v map f
  def flatMap[F[_], A, B](v: Free[F, A])(f: A => Free[F, B]): Free[F, B] = v flatMap f
  def apply[F[_], A, B](ff: Free[F, A => B])(fa: Free[F, A]): Free[F, B] = fa apply ff

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Effect(fa)
}

trait FreeImplicits {
  implicit def freeFunctor[F[_]]: Functor[({type f[x] = Free[F, x]})#f] =
    new Functor[({type f[x] = Free[F, x]})#f] {
      def map[A, B](v: Free[F, A])(f: A => B): Free[F, B] = v map f
    }

  implicit def freeApplicative[F[_]]: Applicative[({type f[x] = Free[F, x]})#f] =
    new Applicative[({type f[x] = Free[F, x]})#f] {
      val functor: Functor[({type f[x] = Free[F, x]})#f] = Functor[({type f[x] = Free[F, x]})#f]
      def pure[A](a: => A): Free[F, A] = Pure(a)
      def apply[A, B](ff: Free[F, A => B])(fa: Free[F, A]): Free[F, B] = fa apply ff
    }

  implicit def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] =
    new Monad[({type f[x] = Free[F, x]})#f] {
      val applicative: Applicative[({type f[x] = Free[F, x]})#f] = Applicative[({type f[x] = Free[F, x]})#f]
      def flatMap[A, B](v: Free[F, A])(f: A => Free[F, B]): Free[F, B] = v flatMap f
    }
}