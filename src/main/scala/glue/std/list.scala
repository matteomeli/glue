package glue.std

import glue.typeclass.{Applicative, Foldable, Functor, Monoid, Traverse}

object list extends ListFunctions with ListSyntax with ListImplicits

trait ListFunctions {
  def empty[A]: List[A] = Nil
}

trait ListSyntax {}

trait ListImplicits {
  import list.empty

  implicit def listIsMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    val unit: List[A] = List.empty
    def combine(l: List[A], r: List[A]): List[A] = l ++ r
  }

  implicit val listIsFoldable: Foldable[List] = new Foldable[List] {
    def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldMap[A, B](as: List[A])(f: A => B)(implicit M: Monoid[B]): B =
      as.foldLeft(M.unit)((b, a) => M.combine(b, f(a)))
  }

  implicit val listIsApplicative: Applicative[List] = new Applicative[List] {
    val functor: Functor[List] = Functor[List]
    def unit[A](a: => A): List[A] = List(a)
    def apply[A, B](f: List[A => B])(as: List[A]): List[B] = as flatMap { a => f.map(_(a)) }
  }

  implicit val listIsTraversable: Traverse[List] = new Traverse[List] {
    val foldable: Foldable[List] = Foldable[List]
    val functor: Functor[List] = Functor[List]
    def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit applicative: Applicative[G]): G[List[B]] =
      as.foldRight(applicative.unit(empty[B])) { (a, gl) =>
        applicative.map2(f(a), gl) { (b, l) => b :: l }
      }
  }

  private implicit def listIsFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }
}
