import scala.language.higherKinds

package object glue {
  type Foldable[F[_]] = typeclass.Foldable[F]
  type Monoid[A] = typeclass.Monoid[A]
  type Show[A] = typeclass.Show[A]
}