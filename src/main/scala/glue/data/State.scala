package glue
package data

import glue.typeclass.{Applicative, Functor, Monad}

trait IndexedStateFunctions {
  def constantIndexedState[S1, S2, A](a: A, s: => S2): IndexedState[S1, S2, A] =
    IndexedState((_: S1) => (s, a))

  def iPut[S1, S2](s: S2): IndexedState[S1, S2, Unit] = IndexedState(_ => (s, ()))

  def iModify[S1, S2](f: S1 => S2): IndexedState[S1, S2, Unit] = IndexedState(s => {
    val r = f(s);
    (r, ())
  })
}

trait StateFunctions {
  def init[S]: State[S, S] = State(s => (s, s))
  def state[S, A](a: A): State[S, A] = State((_ : S, a))
  def constantState[S, A](a: A, s: => S): State[S, A] = State((_: S) => (s, a))

  def get[S]: State[S, S] = init
  def gets[S, T](f: S => T): State[S, T] = State(s => (s, f(s)))
  def set[S](s: S): State[S, Unit] = State(_ => (s, ()))
  // Synonym for set
  def put[S](s: S): State[S, Unit] = set(s)
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  // TODO: How do you add these to State, considering it's only a type alias? They would need to go in IndexedStateT... but is that possible?
  //def map2[S, A, B, C](s1: State[S, A], s2: State[S, B])(f: (A, B) => C): State[S, C] = s1.map2(s2)(f)
  //def sequence[S, A](as: List[State[S, A]]): State[S, List[A]] = as.foldRight[State[S, List[A]]](init(List())) { (s, l) => s.map2(l)(_ :: _) }
}

trait StateImplicits {
  private implicit def stateIsFunctor[S]: Functor[({type f[x] = State[S, x]})#f] = new Functor[({type f[x] = State[S, x]})#f] {
    def map[A, B](s: State[S, A])(f: A => B): State[S, B] = s map f
  }

  private implicit def stateIsApplicative[S]: Applicative[({type f[x] = State[S, x]})#f] = new Applicative[({type f[x] = State[S, x]})#f] {
    val functor: Functor[({type f[x] = State[S, x]})#f] = Functor[({type f[x] = State[S, x]})#f]
    def pure[A](a: => A): State[S, A] = State.state(a)
    def apply[A, B](fs: State[S, A => B])(sa: State[S, A]): State[S, B] = for {
      a <- sa
      f <- fs
    } yield f(a)
  }

  implicit def stateIsMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    val applicative: Applicative[({type f[x] = State[S, x]})#f] = Applicative[({type f[x] = State[S, x]})#f]
    def flatMap[A, B](s: State[S, A])(f: A => State[S, B]): State[S, B] = s flatMap f
  }
}
