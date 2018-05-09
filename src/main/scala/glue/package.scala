package object glue {
  // Typeclasses
  type Applicative[F[_]] = glue.typeclass.Applicative[F]
  type Eq[A] = glue.typeclass.Eq[A]
  type Foldable[F[_]] = glue.typeclass.Foldable[F]
  type Functor[F[_]] = glue.typeclass.Functor[F]
  type Monad[F[_]] = glue.typeclass.Monad[F]
  type Monoid[A] = glue.typeclass.Monoid[A]
  type Show[A] = glue.typeclass.Show[A]
  type Traverse[F[_]] = glue.typeclass.Traverse[F]

  val Applicative = glue.typeclass.Applicative
  val Eq = glue.typeclass.Eq
  val Foldable = glue.typeclass.Foldable
  val Functor = glue.typeclass.Functor
  val Monad = glue.typeclass.Monad
  val Monoid = glue.typeclass.Monoid
  val Show = glue.typeclass.Show
  val Traverse = glue.typeclass.Traverse

  // Data
  type Const[A, B] = glue.data.Const[A, B]
  type Identity[A] = glue.data.Identity[A]
  type Reader[R, A] = glue.data.Reader[R, A]
  type State[S, A] = glue.data.State[S, A]
  type Writer[W, A] = glue.data.Writer[W, A]

  val Identity = glue.data.Identity
  val Const = glue.data.Const
  val Reader = glue.data.Reader
  val State = glue.data.State
  val Writer = glue.data.Writer

  // Data aliases
  type Id[A] = A
}
