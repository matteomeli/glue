package glue.typeclass

case class Identity[A](run: A) extends AnyVal

object Identity {
  type Id[A] = A

  object syntax extends IdentitySyntax
}

trait IdentitySyntax {
  implicit class IdOps[A](a: A) {
    def unused(): Unit = ()
  }
}

trait IdentityImplicits {
  import Identity._

  implicit val idIsFunctor: Functor[Id] = new Functor[Id] {
    def map[A, B](id: Id[A])(f: A => B): Id[B] = f(id)
  }

  implicit val idIsApplicative: Applicative[Id] = new Applicative[Id] {
    val functor: Functor[Id] = Functor[Id]
    def unit[A](a: => A): Id[A] = a
    def apply[A, B](f: Id[A => B])(fa: Id[A]): Id[B] = f(fa)
  }
}