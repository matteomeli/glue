package glue
package data

trait ReaderFunctions {
  def apply[A, B](f: A => B): Reader[A, B] = ReaderT[Id, A, B](f)
  def read[R]: Reader[R, R] = ReaderT[Id, R, R](identity)
}

trait ReaderImplicits {
  private implicit def readerIsFunctor[R]: Functor[({type f[x] = Reader[R, x]})#f] = new Functor[({type f[x] = Reader[R, x]})#f] {
    def map[A, B](ra: Reader[R, A])(f: A => B): Reader[R, B] = ra map f
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

  implicit def readerIsMonad[R]: Monad[({type f[x] = Reader[R, x]})#f] = new Monad[({type f[x] = Reader[R, x]})#f] {
    val applicative: Applicative[({type f[x] = Reader[R, x]})#f] = Applicative[({type f[x] = Reader[R, x]})#f]
    def flatMap[A, B](ra: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = ra flatMap f
  }
}
