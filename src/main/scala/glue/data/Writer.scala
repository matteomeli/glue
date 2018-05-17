package glue
package data

import glue.typeclass.{Applicative, Functor, Monad, Monoid}

final case class Writer[W, A](run: (W, A)) {
  def written: W = run._1
  def value: A = run._2

  def map[B](f: A => B): Writer[W, B] = Writer((run._1, f(run._2)))
  def flatMap[B](f: A => Writer[W, B])(implicit M: Monoid[W]): Writer[W, B] = Writer {
    val w = f(run._2)
    (M.combine(run._1, w.run._1), w.run._2)
  }
}

object Writer extends WriterFunctions {
  def apply[W, A](w: W, a: A): Writer[W, A] = writer((w, a))

  object implicits extends WriterImplicits
}

trait WriterFunctions {
  def writer[W, A](wa: (W, A)): Writer[W, A] = Writer(wa)
  def tell[W](w: W): Writer[W, Unit] = Writer((w, ()))
  def written[W, A](w: Writer[W, A]): W = w.written
  def value[W, A](w: Writer[W, A]): A = w.value

  def map[W, A, B](w: Writer[W, A])(f: A => B): Writer[W, B] = w map f
  def flatMap[W: Monoid, A, B](w: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = w flatMap f
}

trait WriterImplicits {
  implicit def writerIsMonad[W: Monoid]: Monad[({type f[x] = Writer[W, x]})#f] = new Monad[({type f[x] = Writer[W, x]})#f] {
    val applicative: Applicative[({type f[x] = Writer[W, x]})#f] = Applicative[({type f[x] = Writer[W, x]})#f]
    def flatMap[A, B](wa: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = wa flatMap f
  }

  private implicit def writerIsApplicative[W: Monoid]: Applicative[({type f[x] = Writer[W, x]})#f] = new Applicative[({type f[x] = Writer[W, x]})#f] {
    val functor: Functor[({type f[x] = Writer[W, x]})#f] = Functor[({type f[x] = Writer[W, x]})#f]
    def pure[A](a: => A): Writer[W, A] = Writer(Monoid[W].unit, a)
    def apply[A, B](wf: Writer[W, A => B])(wa: Writer[W, A]): Writer[W, B] = Writer {
      (Monoid[W].combine(wf.written, wa.written), wf.value(wa.value))
    }
  }

  private implicit def writerIsFunctor[W]: Functor[({type f[x] = Writer[W, x]})#f] = new Functor[({type f[x] = Writer[W, x]})#f] {
    def map[A, B](wa: Writer[W, A])(f: A => B): Writer[W, B] = wa map f
  }
}
