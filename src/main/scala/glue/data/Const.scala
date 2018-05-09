package glue
package data

import glue.typeclass.{Functor, Show}

final case class Const[A, B](run: A)

object Const {
  object implicits extends ConstImplicits
}

trait ConstImplicits {
  implicit def constIsFunctor[C]: Functor[({type f[x] = Const[C, x]})#f] =
    new Functor[({type f[x] = Const[C, x]})#f] {
      def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] = Const(fa.run)
    }

  implicit def constCanShow[A, B]: Show[Const[A, B]] = Show.fromToString[Const[A, B]]
}
