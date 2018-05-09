package glue
package data

import glue.typeclass.{Applicative, Functor, Monad}

final case class State[S, A](run: S => (S, A)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    val (s1, a) = run(s)
    (s1, f(a))
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s =>
    val (s1, a) = run(s)
    f(a).run(s1)
  }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State { s =>
    val (s1, a) = run(s)
    val (s2, b) = sb.run(s1)
    (s2, f(a, b))
  }
}

object State extends StateFunctions {
  object implicits extends StateImplicits
}

trait StateFunctions {
  def init[S, A](a: => A): State[S, A] = State(s => (s, a))
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def map[S, A, B](s: State[S, A])(f: A => B): State[S, B] = s.map(f)
  def flatMap[S, A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] = s.flatMap(f)
  def map2[S, A, B, C](s1: State[S, A], s2: State[S, B])(f: (A, B) => C): State[S, C] = s1.map2(s2)(f)
  def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] =
    as.foldRight[State[S, List[A]]](init(List())) { (s, l) => s.map2(l)(_ :: _) }
}

trait StateImplicits {
  implicit def stateIsMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    val applicative: Applicative[({type f[x] = State[S, x]})#f] = Applicative[({type f[x] = State[S, x]})#f]
    def flatMap[A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] = s flatMap f
  }

  private implicit def stateIsApplicative[S]: Applicative[({type f[x] = State[S, x]})#f] = new Applicative[({type f[x] = State[S, x]})#f] {
    val functor: Functor[({type f[x] = State[S, x]})#f] = Functor[({type f[x] = State[S, x]})#f]
    def unit[A](a: => A): State[S, A] = State.init(a)
    def apply[A, B](fs: State[S, A => B])(sa: State[S, A]): State[S, B] = for {
      a <- sa
      f <- fs
    } yield f(a)
  }

  implicit def stateIsFunctor[S]: Functor[({type f[x] = State[S, x]})#f] = new Functor[({type f[x] = State[S, x]})#f] {
    def map[A, B](s: State[S, A])(f: A => B): State[S, B] = s map f
  }
}
