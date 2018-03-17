package glue

trait Typeclasses {
  type Monoid[T] = typeclass.Monoid[T]
}

trait Syntax
  extends typeclass.MonoidSyntax
  with typeclass.ShowSyntax

trait Instances
  extends typeclass.MonoidInstances
  with typeclass.ShowInstances

trait Prelude extends Typeclasses with Syntax with Instances

object Glue extends Prelude
