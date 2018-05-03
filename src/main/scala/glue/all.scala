package glue

trait AllTypeclasses {
  type Applicative[F[_]] = typeclass.Applicative[F]
  type Eq[A] = typeclass.Eq[A]
  type Foldable[F[_]] = typeclass.Foldable[F]
  type Functor[F[_]] = typeclass.Functor[F]
  type Monoid[A] = typeclass.Monoid[A]
  type Show[A] = typeclass.Show[A]
  type Traverse[F[_]] = typeclass.Traverse[F]

  val Applicative = typeclass.Applicative
  val Eq = typeclass.Eq
  val Foldable = typeclass.Foldable
  val Functor = typeclass.Functor
  val Monoid = typeclass.Monoid
  val Show = typeclass.Show
  val Traverse = typeclass.Traverse
}

trait AllData {
  type Const[A, B] = data.Const[A, B]
  type Identity[A] = data.Identity[A]

  val Identity = data.Identity
  val Const = data.Const
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
  with typeclass.TraverseFunctions

trait AllSyntax
  extends std.AllSyntax
  with data.ConstSyntax
  with data.IdentitySyntax
  with typeclass.ApplicativeSyntax
  with typeclass.EqSyntax
  with typeclass.FoldableSyntax
  with typeclass.FunctorSyntax
  with typeclass.MonoidSyntax
  with typeclass.ShowSyntax
  with typeclass.TraverseSyntax

trait AllImplicits
  extends std.AllImplicits
  with data.ConstImplicits
  with data.IdentityImplicits
  with typeclass.MonoidImplicits

object functions extends AllFunctions

object syntax extends AllSyntax

object implicits extends Hierarchy with AllImplicits

object prelude extends Hierarchy with AllData with AllFunctions

object all extends Hierarchy with AllFunctions with AllSyntax with AllImplicits
