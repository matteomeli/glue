package glue.std

import glue.typeclass.{Foldable, Monoid}

object indexedSeq extends IndexedSeqFunctions with IndexedSeqSyntax with IndexedSeqInstances

trait IndexedSeqFunctions {
  def foldMapB[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    if (as.length == 0) mb.unit
    else if (as.length == 1) f(as(0))
    else {
      val (left, right) = as.splitAt(as.length / 2)
      mb.combine(foldMapB(left)(f)(mb), foldMapB(right)(f)(mb))
    }
}

trait IndexedSeqSyntax {
  implicit class IndexedSeqOps[A](self: IndexedSeq[A]) {
    def foldMapB[B](f: A => B)(mb: Monoid[B]): B = indexedSeq.foldMapB(self)(f)(mb)
  }
}

trait IndexedSeqInstances {
  implicit val indexedSeqIsFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)
    def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B =
      as.foldRight(z)(f)
    def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(implicit M: Monoid[B]): B =
      indexedSeq.foldMapB(as)(f)(M)
  }
}