package glue
package data

import glue.typeclass.{Applicative, Functor, Monad, Monoid}

trait WriterFunctions {
  import glue.implicits._

  def apply[W, A](wa: (W, A)): Writer[W, A] = WriterT[Id, W, A](wa)
  def writer[W, A](w: W, a: A): Writer[W, A] = Writer((w, a))
  def tell[W](w: W): Writer[W, Unit] = WriterT.tell[Id, W](w)
  def value[W: Monoid, A](a: A): Writer[W, A] = WriterT.value(a)
}

trait WriterImplicits {
  implicit def writerIsMonad[W: Monoid]: Monad[({type f[x] = Writer[W, x]})#f] = new Monad[({type f[x] = Writer[W, x]})#f] {
    val applicative: Applicative[({type f[x] = Writer[W, x]})#f] = Applicative[({type f[x] = Writer[W, x]})#f]
    def flatMap[A, B](wa: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = wa flatMap f
  }

  private implicit def writerIsApplicative[W: Monoid]: Applicative[({type f[x] = Writer[W, x]})#f] = new Applicative[({type f[x] = Writer[W, x]})#f] {
    val functor: Functor[({type f[x] = Writer[W, x]})#f] = Functor[({type f[x] = Writer[W, x]})#f]
    def pure[A](a: => A): Writer[W, A] = Writer((Monoid[W].unit, a))
    def apply[A, B](wf: Writer[W, A => B])(wa: Writer[W, A]): Writer[W, B] = Writer {
      val fab = wf.value
      val a = wa.value
      (Monoid[W].combine(wf.written, wa.written), fab(a))
    }
  }

  private implicit def writerIsFunctor[W]: Functor[({type f[x] = Writer[W, x]})#f] = new Functor[({type f[x] = Writer[W, x]})#f] {
    def map[A, B](wa: Writer[W, A])(f: A => B): Writer[W, B] = wa map f
  }
}
