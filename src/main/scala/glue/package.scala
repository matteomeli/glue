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
  type EitherT[F[_], A, B] = glue.data.EitherT[F, A, B]
  type Identity[A] = glue.data.Identity[A]
  type IdT[F[_], A] = glue.data.IdT[F, A]
  type IndexedStateT[F[_], S, T, A] = glue.data.IndexedStateT[F, S, T, A]
  type Kleisli[F[_], A, B] = glue.data.Kleisli[F, A, B]
  type OptionT[F[_], A] = glue.data.OptionT[F, A]
  type Reader[R, A] = glue.data.Reader[R, A]
  type Writer[W, A] = glue.data.Writer[W, A]
  type StateT[F[_], S, A] = glue.data.StateT[F, S, A]
  type State[S, A] = glue.data.State[S, A]

  val Const = glue.data.Const
  val EitherT = glue.data.EitherT
  val Identity = glue.data.Identity
  val IdT = glue.data.IdT
  val IndexedStateT = glue.data.IndexedStateT
  val Kleisli = glue.data.Kleisli
  val OptionT = glue.data.OptionT
  val Reader = glue.data.Reader
  val Writer = glue.data.Writer
  val StateT = glue.data.StateT
  val State = glue.data.State

  // Id
  type Id[A] = A
  implicit class IdOps[A](a: A) {
    def unused(): Unit = ()
  }
  implicit val idIsfunctor: Functor[Id] = new Functor[Id] {
    def map[A, B](a: Id[A])(f: A => B): Id[B] = f(a)
  }
  private implicit val idIsApplicative: Applicative[Id] = new Applicative[Id] {
    val functor: Functor[Id] = Functor[Id]
    def pure[A](a: => A): Id[A] = a
    def apply[A, B](f: Id[A => B])(a: Id[A]): Id[B] = f(a)
  }
  implicit val idIsMonad: Monad[Id] = new Monad[Id] {
    val applicative: Applicative[Id] = Applicative[Id]
    def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }
  private implicit val idIsFoldable: Foldable[Id] = new Foldable[Id] {
    def foldLeft[A, B](a: Id[A], z: B)(f: (B, A) => B): B = f(z, a)
    def foldRight[A, B](a: Id[A], z: B)(f: (A, B) => B): B = f(a, z)
    def foldMap[A, B](a: Id[A])(f: A => B)(implicit M: Monoid[B]): B = M.combine(M.unit, f(a))
  }
  implicit val idIsTraversable: Traverse[Id] = new Traverse[Id] {
    val foldable: Foldable[Id] = Foldable[Id]
    val functor: Functor[Id] = Functor[Id]
    def traverse[G[_], A, B](a: Id[A])(f: A => G[B])(implicit G: Applicative[G]): G[Id[B]] = f(a)
  }
}
