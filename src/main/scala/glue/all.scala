package glue

trait AllTypeclasses {
  type Applicative[F[_]] = typeclass.Applicative[F]
  type Eq[A] = typeclass.Eq[A]
  type Foldable[F[_]] = typeclass.Foldable[F]
  type Functor[F[_]] = typeclass.Functor[F]
  type Monoid[A] = typeclass.Monoid[A]
  type Show[A] = typeclass.Show[A]

  val Applicative = typeclass.Applicative
  val Eq = typeclass.Eq
  val Foldable = typeclass.Foldable
  val Functor = typeclass.Functor
  val Monoid = typeclass.Monoid
  val Show = typeclass.Show
}

trait AllData {
  type Identity[A] = data.Identity[A]
  val Identity = data.Identity
}

trait AllDataAliases {
  type Id[A] = A
}

trait AllFunctions
  extends std.AllFunctions
  with typeclass.ApplicativeFunctions
  with typeclass.EqFunctions
  with typeclass.FoldableFunctions
  with typeclass.FunctorFunctions
  with typeclass.MonoidFunctions
  with typeclass.ShowFunctions

trait AllSyntax
  extends std.AllSyntax
  with data.IdentitySyntax
  with typeclass.ApplicativeSyntax
  with typeclass.EqSyntax
  with typeclass.FoldableSyntax
  with typeclass.FunctorSyntax
  with typeclass.MonoidSyntax
  with typeclass.ShowSyntax

trait AllImplicits
  extends std.AllImplicits
  with data.IdentityImplicits
  with typeclass.MonoidImplicits

object functions extends AllFunctions

object syntax extends AllSyntax

object implicits extends Hierarchy with AllImplicits

object all extends Hierarchy with AllFunctions with AllSyntax with AllImplicits
