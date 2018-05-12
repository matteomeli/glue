package glue
package data

import glue.typeclass.{Applicative, Functor, Monad}

final case class Reader[R, A](run: R => A) {
  def map[B](f: A => B): Reader[R, B] = Reader(r => f(run(r)))
  def flatMap[B](f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(run(r)).run(r))
  def map2[B, C](rb: Reader[R, B])(f: (A, B) => C): Reader[R, C] = Reader { r =>
    val a = run(r)
    val b = rb.run(r)
    f(a, b)
  }
}

object Reader extends ReaderFunctions {
  object implicits extends ReaderImplicits
}

trait ReaderFunctions {
  def apply[R, A](f: R => A): Reader[R, A] = Reader(f)
  def read[R]: Reader[R, R] = Reader(identity)

  def map[R, A, B](r: Reader[R, A])(f: A => B): Reader[R, B] = r.map(f)
  def flatMap[R, A, B](r: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = r.flatMap(f)
  def map2[R, A, B, C](ra: Reader[R, A], rb: Reader[R, B])(f: (A, B) => C): Reader[R, C] = ra.map2(rb)(f)
}

trait ReaderImplicits {
  implicit def readerIsMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] = new Monad[({type f[x] = Reader[R, x]})#f] {
    val applicative: Applicative[({type f[x] = Reader[R, x]})#f] = Applicative[({type f[x] = Reader[R, x]})#f]
    def flatMap[A, B](ra: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ra flatMap f
  }

  private implicit def readerIsApplicative[R]: Applicative[({type f[x] = Reader[R, x]})#f] = new Applicative[({type f[x] = Reader[R, x]})#f] {
    val functor: Functor[({type f[x] = Reader[R, x]})#f] = Functor[({type f[x] = Reader[R, x]})#f]
    def pure[A](a: => A): Reader[R, A] = Reader(_ => a)
    def apply[A, B](rf: Reader[R, A => B])(ra: Reader[R, A]): Reader[R, B] = Reader { r =>
      val a = ra.run(r)
      val f = rf.run(r)
      f(a)
    }

    override def replicateM[A](n: Int, ra: Reader[R, A]): Reader[R, List[A]] = Reader { r =>
      val a = ra.run(r)
      List.fill(n)(a)
    }
  }

  private implicit def readerIsFunctor[R]: Functor[({type f[x] = Reader[R, x]})#f] = new Functor[({type f[x] = Reader[R, x]})#f] {
    def map[A, B](ra: Reader[R, A])(f: A => B): Reader[R, B] = ra map f
  }
}
