package glue
package data

import std.option._

sealed trait Validation[+E, +A] {
  def map[B](f: A => B): Validation[E, B] = this match {
    case f @ Failure(_, _) => f
    case Success(a) => Success(f(a))
  }

  def flatMap[D >: E, B](f: A => Validation[D, B]): Validation[D, B] = this match {
    case e @ Failure(_, _) => e
    case Success(a) => f(a)
  }

  def fold[B](z: => B)(f: A => B): B = this match {
    case Success(a) => f(a)
    case _ => z
  }

  def isValid: Boolean = fold(false)(_ => true)

  def isInvalid: Boolean = !isValid

  def exists(p: A => Boolean): Boolean = fold(false)(p(_))

  def getOrElse[AA >: A](z: => AA): AA = fold(z)(identity)

  def orElse[EE >: E, AA >: A](z: => Validation[EE, AA]): Validation[EE, AA] = fold(z)(Success(_))

  def filterOrElse[EE >: E](p: A => Boolean, e: => EE): Validation[EE, A] = this match {
    case s @ Success(a) if p(a) => s
    case Success(_) => Failure(e)
    case f @ Failure(_, _) => f
  }

  def toOption: Option[A] = fold(none[A])(some(_))

  def totEither: Either[Vector[E], A] = this match {
    case Success(a) => Right(a)
    case Failure(x, xs) => Left(x +: xs)
  }
}

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

object Validation {
  object implicits extends ValidationImplicits
}

trait ValidationImplicits {
  private implicit def validationIsFunctor[E]: Functor[({type f[x] = Validation[E, x]})#f] =
    new Functor[({type f[x] = Validation[E, x]})#f] {
      def map[A, B](va: Validation[E, A])(f: A => B): Validation[E, B] = va map f
    }

  private implicit def validationIsApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      val functor: Functor[({type f[x] = Validation[E, x]})#f] = Functor[({type f[x] = Validation[E, x]})#f]
      def pure[A](a: => A): Validation[E, A] = Success(a)
      def apply[A, B](vf: Validation[E, A => B])(va: Validation[E, A]): Validation[E, B] = (vf, va) match {
        case (Failure(x, xs), Failure(y, ys)) => Failure(x, xs ++ Vector(y) ++ ys)
        case (e @ Failure(_, _), _) => e
        case (_, e @ Failure(_, _)) => e
        case (Success(fab), Success(a)) => Success(fab(a))
      }
    }

  implicit def validationIsMonad[E]: Monad[({type f[x] = Validation[E, x]})#f] =
    new Monad[({type f[x] = Validation[E, x]})#f] {
      val applicative: Applicative[({type f[x] = Validation[E, x]})#f] = Applicative[({type f[x] = Validation[E, x]})#f]
      def flatMap[A, B](v: Validation[E, A])(f: A => Validation[E, B]): Validation[E, B] = v flatMap f
    }

  implicit def validationIsFoldable[E]: Foldable[({type f[x] = Validation[E, x]})#f] =
    new Foldable[({type f[x] = Validation[E, x]})#f] {
      def foldLeft[A, B](v: Validation[E, A], z: B)(f: (B, A) => B): B = v match {
        case Failure(_, _) => z
        case Success(a) => f(z, a)
      }
      def foldRight[A, B](v: Validation[E, A], z: B)(f: (A, B) => B): B = v match {
        case Failure(_, _) => z
        case Success(a) => f(a, z)
      }
      def foldMap[A, B](v: Validation[E, A])(f: A => B)(implicit M: Monoid[B]): B = v match {
        case Failure(_, _) => M.unit
        case Success(a) => f(a)
      }
    }

  implicit def validationIsTraversable[E]: Traverse[({type f[x] = Validation[E, x]})#f] =
    new Traverse[({type f[x] = Validation[E, x]})#f] {
      val foldable: Foldable[({type f[x] = Validation[E, x]})#f] = Foldable[({type f[x] = Validation[E, x]})#f]
      val functor: Functor[({type f[x] = Validation[E, x]})#f] = Functor[({type f[x] = Validation[E, x]})#f]
      def traverse[G[_], A, B](v: Validation[E, A])(f: A => G[B])(implicit G: Applicative[G]): G[Validation[E, B]] = v match {
        case e @ Failure(_, _) => G.pure(e)
        case Success(a) => G.map(f(a))(Success(_))
      }
    }
}
