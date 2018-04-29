package glue

import typeclass._

trait AllFunctions
  extends ApplicativeFunctions
  with EqFunctions
  with FoldableFunctions
  with FunctorFunctions
  with MonoidFunctions
  with ShowFunctions

trait AllSyntax
  extends IdentitySyntax
  with ApplicativeSyntax
  with EqSyntax
  with FoldableSyntax
  with FunctorSyntax
  with MonoidSyntax
  with ShowSyntax

trait AllImplicits
  extends IdentityImplicits
  with MonoidImplicits

object functions extends AllFunctions

object syntax extends AllSyntax

object implicits extends AllImplicits

object all extends AllFunctions with AllSyntax with AllImplicits
  with std.AllFunctions with std.AllSyntax with std.AllImplicits
