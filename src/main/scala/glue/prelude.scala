package glue

import typeclass._

trait AllSyntax
  extends FoldableSyntax
  with FunctorSyntax
  with MonoidSyntax
  with ShowSyntax

trait AllInstances
  extends FoldableInstances
  with FunctorInstances
  with MonoidInstances
  with ShowInstances

object prelude extends AllSyntax with AllInstances
