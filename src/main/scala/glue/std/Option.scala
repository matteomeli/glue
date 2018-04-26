package glue.std

import glue.typeclass.{Foldable, Functor, Monoid}

object option extends OptionFunctions with OptionSyntax with OptionInstances

trait OptionFunctions {
  final def some[A](a: A): Option[A] = Some(a)
  final def none[A]: Option[A] = None
}

trait OptionSyntax {}

trait OptionInstances {
  implicit def optionIsMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    val unit: Option[A] = None
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  }

  implicit val optionIsFoldable: Foldable[Option] = new Foldable[Option] {
    def foldLeft[A, B](oa: Option[A])(z: B)(f: (B, A) => B): B = oa.map(f(z, _)).getOrElse(z)
    def foldRight[A, B](oa: Option[A])(z: B)(f: (A, B) => B): B = oa.map(f(_, z)).getOrElse(z)
    def foldMap[A, B](oa: Option[A])(f: A => B)(implicit M: Monoid[B]): B =
      oa.map(a => M.combine(f(a), M.unit)).getOrElse(M.unit)
  }

  implicit val optionIsFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](o: Option[A])(f: A => B): Option[B] = o map f
  }
}
