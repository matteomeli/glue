package glue

import typeclass._

trait AllSyntax
  extends FoldableSyntax
  with MonoidSyntax
  with ShowSyntax

trait AllInstances
  extends FoldableInstances
  with MonoidInstances
  with ShowInstances

object prelude extends AllSyntax with AllInstances
