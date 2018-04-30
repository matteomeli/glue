package glue

trait Hierarchy extends Hierarchy.Hierarchy0

object Hierarchy {
  trait Hierarchy0 {
    implicit def applicativeIsFunctor[F[_]: Applicative]: Functor[F] =
      Applicative[F].functor
  }
}
