package glue

import typeclass._

trait AllFunctions
  extends FoldableFunctions
  with FunctorFunctions
  with MonoidFunctions
  with ShowFunctions

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

object prelude extends AllFunctions with AllSyntax with AllInstances with std.AllFunctions
