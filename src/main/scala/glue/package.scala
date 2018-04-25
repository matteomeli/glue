import scala.language.higherKinds

package object glue {
  type Id[A] = typeclass.Identity.Id[A]

  type Foldable[F[_]] = typeclass.Foldable[F]
  type Functor[F[_]] = typeclass.Functor[F]
  type Monoid[A] = typeclass.Monoid[A]
  type Show[A] = typeclass.Show[A]

  val Foldable = typeclass.Foldable
  val Functor = typeclass.Functor
  val Monoid = typeclass.Monoid
  val Show = typeclass.Show
}