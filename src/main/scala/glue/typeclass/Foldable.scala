package glue.typeclass

import scala.language.higherKinds

trait Foldable[F[_]] {
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(implicit M: Monoid[B]): B

  def combine[A](as: F[A])(implicit M: Monoid[A]): A = foldLeft(as)(M.unit)(M.combine)
  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List[A]())((t, h) => h :: t).reverse
}

object Foldable extends FoldableFunctions {
  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F

  object syntax extends FoldableSyntax
}

trait FoldableFunctions {
  def foldLeft[F[_]: Foldable, A, B](fa: F[A])(z: B)(f: (B, A) => B): B = Foldable[F].foldLeft(fa)(z)(f)
  def foldRight[F[_]: Foldable, A, B](fa: F[A])(z: B)(f: (A, B) => B): B = Foldable[F].foldRight(fa)(z)(f)
  def foldMap[F[_]: Foldable, A, B: Monoid](fa: F[A])(f: A => B): B = Foldable[F].foldMap(fa)(f)
}

trait FoldableSyntax {
  implicit class FoldableOps[F[_]: Foldable, A](self: F[A]) {
    def foldLeft[B](z: B)(f: (B, A) => B): B = Foldable[F].foldLeft(self)(z)(f)
    def foldRight[B](z: B)(f: (A, B) => B): B = Foldable[F].foldRight(self)(z)(f)
    def foldMap[B](f: A => B)(implicit M: Monoid[B]): B = Foldable[F].foldMap(self)(f)

    def combine(implicit M: Monoid[A]): A = Foldable[F].combine(self)
    def toList: List[A] = Foldable[F].toList(self)
  }
}
