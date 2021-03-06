package glue
package std

import glue.typeclass.{Applicative, Foldable, Functor, Monad, Monoid, Traverse}

object list extends ListFunctions with ListImplicits

trait ListFunctions {
  def empty[A]: List[A] = Nil
}

trait ListImplicits {
  import list.empty

  private implicit val listIsFoldable: Foldable[List] = new Foldable[List] {
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    def foldMap[A, B](as: List[A])(f: A => B)(implicit M: Monoid[B]): B =
      as.foldLeft(M.unit)((b, a) => M.combine(b, f(a)))
  }

  private implicit val listIsFunctor: Functor[List] = new Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }

  implicit val listIsTraversable: Traverse[List] = new Traverse[List] {
    val foldable: Foldable[List] = Foldable[List]
    val functor: Functor[List] = Functor[List]
    def traverse[G[_], A, B](as: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      as.foldLeft(G.pure(empty[B])) { (gl, a) =>
        G.map2(gl, f(a)) { (l, b) => b :: l }
      }
  }

  private implicit val listIsApplicative: Applicative[List] = new Applicative[List] {
    val functor: Functor[List] = Functor[List]
    def pure[A](a: => A): List[A] = List(a)
    def apply[A, B](f: List[A => B])(as: List[A]): List[B] =
      if (f.isEmpty) Nil
      else as flatMap { a => f.map(_(a)) }
  }

  implicit val listIsMonad: Monad[List] = new Monad[List] {
    val applicative: Applicative[List] = Applicative[List]
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as flatMap f
    override def map[A, B](as: List[A])(f: A => B): List[B] = as map f
    override def map2[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] =
      if (bs.isEmpty) Nil
      else as.flatMap(a => bs.map(b => f(a, b)))
  }

  implicit def listIsMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    val unit: List[A] = List.empty
    def combine(l: List[A], r: List[A]): List[A] = l ++ r
  }
}
