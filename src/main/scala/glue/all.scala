package glue

trait AllFunctions
  extends std.AllFunctions
  with data.AllFunctions
  with typeclass.ApplicativeFunctions
  with typeclass.EqFunctions
  with typeclass.FoldableFunctions
  with typeclass.FunctorFunctions
  with typeclass.MonadFunctions
  with typeclass.MonoidFunctions
  with typeclass.ShowFunctions
  with typeclass.TraverseFunctions

trait AllSyntax
  extends typeclass.ApplicativeSyntax
  with typeclass.EqSyntax
  with typeclass.FoldableSyntax
  with typeclass.FunctorSyntax
  with typeclass.MonadSyntax
  with typeclass.MonoidSyntax
  with typeclass.ShowSyntax
  with typeclass.TraverseSyntax

trait AllImplicits
  extends std.AllImplicits
  with data.AllImplicits
  with typeclass.MonoidImplicits

object functions extends AllFunctions

object syntax extends AllSyntax

object implicits extends Hierarchy with AllImplicits

object all extends Hierarchy with AllFunctions with AllSyntax with AllImplicits
