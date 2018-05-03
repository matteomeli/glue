package glue

trait Hierarchy extends Hierarchy.Hierarchy0

object Hierarchy {
  trait Hierarchy0 extends Hierarchy1 {
    implicit def applicativeIsFunctor[F[_]: Applicative]: Functor[F] =
      Applicative[F].functor

    implicit def traverseIsFoldable[F[_]: Traverse]: Foldable[F] =
      Traverse[F].foldable
  }

  trait Hierarchy1 {
    implicit def traverseIsFunctor[F[_]: Traverse]: Functor[F] =
      Traverse[F].functor
  }
}
