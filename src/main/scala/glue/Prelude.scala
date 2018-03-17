package glue

import scala.language.higherKinds

trait Typeclasses {
  type Foldable[F[_]] = typeclass.Foldable[F]
  type Monoid[A] = typeclass.Monoid[A]
  type Show[A] = typeclass.Show[A]
}

trait Syntax
  extends typeclass.FoldableSyntax
  with typeclass.MonoidSyntax
  with typeclass.ShowSyntax

trait Instances
  extends typeclass.FoldableInstances
  with typeclass.MonoidInstances
  with typeclass.ShowInstances

trait Prelude extends Typeclasses with Syntax with Instances

object Glue extends Prelude
