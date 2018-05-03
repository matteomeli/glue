package glue.data

import glue.typeclass.{Applicative, Functor}

final case class Identity[A](run: A) extends AnyVal

object Identity {
  object syntax extends IdentitySyntax

  object implicits extends IdentityImplicits
}

trait IdentitySyntax {
  implicit class IdOps[A](a: A) {
    def unused(): Unit = ()
  }
}

trait IdentityImplicits {
  implicit val idIsApplicative: Applicative[Identity] = new Applicative[Identity] {
    val functor: Functor[Identity] = Functor[Identity]
    def unit[A](a: => A): Identity[A] = Identity(a)
    def apply[A, B](f: Identity[A => B])(fa: Identity[A]): Identity[B] = Identity(f.run(fa.run))
  }

  private implicit def idIsFunctor: Functor[Identity] = new Functor[Identity] {
    def map[A, B](id: Identity[A])(f: A => B): Identity[B] = Identity(f(id.run))
  }
}
