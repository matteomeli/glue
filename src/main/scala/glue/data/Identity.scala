package glue.data

import glue.typeclass.{Applicative, Foldable, Functor, Monad, Monoid, Traverse}

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
  implicit val idIsFoldable: Foldable[Identity] = new Foldable[Identity] {
    def foldLeft[A, B](ia: Identity[A])(z: B)(f: (B, A) => B): B = f(z, ia.run)
    def foldRight[A, B](ia: Identity[A])(z: B)(f: (A, B) => B): B = f(ia.run, z)
    def foldMap[A, B](ia: Identity[A])(f: A => B)(implicit M: Monoid[B]): B = M.combine(M.unit, f(ia.run))
  }

  implicit val isIsMonad: Monad[Identity] = new Monad[Identity] {
    val applicative: Applicative[Identity] = Applicative[Identity]
    def flatMap[A, B](ia: Identity[A])(f: A => Identity[B]): Identity[B] = f(ia.run)
  }

  implicit def idIsTraversable: Traverse[Identity] = new Traverse[Identity] {
    val foldable: Foldable[Identity] = Foldable[Identity]
    val functor: Functor[Identity] = Functor[Identity]
    def traverse[G[_], A, B](ia: Identity[A])(f: A => G[B])(implicit G: Applicative[G]): G[Identity[B]] =
      G.map(f(ia.run))(Identity(_))
  }

  private implicit def idIsApplicative: Applicative[Identity] = new Applicative[Identity] {
    val functor: Functor[Identity] = Functor[Identity]
    def unit[A](a: => A): Identity[A] = Identity(a)
    def apply[A, B](f: Identity[A => B])(fa: Identity[A]): Identity[B] = Identity(f.run(fa.run))
  }

  private implicit def idIsFunctor: Functor[Identity] = new Functor[Identity] {
    def map[A, B](id: Identity[A])(f: A => B): Identity[B] = Identity(f(id.run))
  }
}
