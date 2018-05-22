package glue
package io

sealed trait IO[A] { self =>
  def run: A

  def map[B](f: A => B): IO[B] =
    new IO[B] { def run = f(self.run) }

  def flatMap[B](f: A => IO[B]): IO[B] =
    new IO[B] { def run = f(self.run).run }

  def map2[B, C](iob: IO[B])(f: (A, B) => C): IO[C] =
    new IO[C] {
      def run = f(self.run, iob.run)
    }
}

object IO extends IOFunctions {
  def apply[A](a: => A): IO[A] = pure(a)

  object implicits extends IOImplicits
}

trait IOFunctions {
  def pure[A](a: => A): IO[A] = new IO[A] { def run = a }
}

trait IOImplicits {
  private implicit val ioIsFunctor: Functor[IO] = new Functor[IO] {
    def map[A, B](ioa: IO[A])(f: A => B): IO[B] = ioa map f
  }

  private implicit val ioIsApplicative: Applicative[IO] = new Applicative[IO] {
    val functor: Functor[IO] = Functor[IO]
    def pure[A](a: => A): IO[A] = IO.pure(a)
    def apply[A, B](iof: IO[A => B])(ioa: IO[A]): IO[B] = iof.map2(ioa) { (f, a) =>
      f(a)
    }
  }

  implicit val ioIsMonad: Monad[IO] = new Monad[IO] {
    val applicative: Applicative[IO] = Applicative[IO]
    def flatMap[A, B](ioa: IO[A])(f: A => IO[B]): IO[B] = ioa flatMap f
  }
}
