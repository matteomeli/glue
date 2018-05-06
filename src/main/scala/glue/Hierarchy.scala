package glue

trait Hierarchy extends Hierarchy.Hierarchy0

object Hierarchy {
  trait Hierarchy0 extends Hierarchy1 {
    implicit def monadIsApplicative[F[_]: Monad]: Applicative[F] =
      Monad[F].applicative

    implicit def monadIsFunctor[F[_]: Monad]: Functor[F] =
      Monad[F].applicative.functor

    implicit def traverseIsFoldable[F[_]: Traverse]: Foldable[F] =
      Traverse[F].foldable
  }

  trait Hierarchy1 extends Hierarchy2 {
    implicit def applicativeIsFunctor[F[_]: Applicative]: Functor[F] =
      Applicative[F].functor
  }

  trait Hierarchy2 {
    implicit def traverseIsFunctor[F[_]: Traverse]: Functor[F] =
      Traverse[F].functor
  }
}
