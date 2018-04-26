package glue

import typeclass._

trait AllFunctions
  extends EqFunctions
  with FoldableFunctions
  with FunctorFunctions
  with MonoidFunctions
  with ShowFunctions

trait AllSyntax
  extends EqSyntax
  with FoldableSyntax
  with FunctorSyntax
  with MonoidSyntax
  with ShowSyntax

trait AllInstances
  extends IdInstances
  with MonoidInstances

object functions extends AllFunctions

object syntax extends AllSyntax

object instances extends AllInstances

object all extends AllFunctions with AllSyntax with AllInstances
  with std.AllFunctions with std.AllSyntax with std.AllInstances
