package glue

import typeclass._

trait syntax
  extends FoldableSyntax
  with MonoidSyntax
  with ShowSyntax

trait instances
  extends FoldableInstances
  with MonoidInstances
  with ShowInstances

object prelude extends syntax with instances
