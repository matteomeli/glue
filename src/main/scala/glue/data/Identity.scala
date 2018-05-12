package glue
package data

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
  implicit val identityIsFoldable: Foldable[Identity] = new Foldable[Identity] {
    def foldLeft[A, B](ia: Identity[A], z: B)(f: (B, A) => B): B = f(z, ia.run)
    def foldRight[A, B](ia: Identity[A], z: B)(f: (A, B) => B): B = f(ia.run, z)
    def foldMap[A, B](ia: Identity[A])(f: A => B)(implicit M: Monoid[B]): B = M.combine(M.unit, f(ia.run))
  }

  implicit val idIsFoldable: Foldable[Id] = new Foldable[Id] {
    def foldLeft[A, B](a: Id[A], z: B)(f: (B, A) => B): B = f(z, a)
    def foldRight[A, B](a: Id[A], z: B)(f: (A, B) => B): B = f(a, z)
    def foldMap[A, B](a: Id[A])(f: A => B)(implicit M: Monoid[B]): B = M.combine(M.unit, f(a))
  }

  implicit val identityIsIsMonad: Monad[Identity] = new Monad[Identity] {
    val applicative: Applicative[Identity] = Applicative[Identity]
    def flatMap[A, B](ia: Identity[A])(f: A => Identity[B]): Identity[B] = f(ia.run)
  }

  implicit val idIsIsMonad: Monad[Id] = new Monad[Id] {
    val applicative: Applicative[Id] = Applicative[Id]
    def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }

  implicit def identityIsTraversable: Traverse[Identity] = new Traverse[Identity] {
    val foldable: Foldable[Identity] = Foldable[Identity]
    val functor: Functor[Identity] = Functor[Identity]
    def traverse[G[_], A, B](ia: Identity[A])(f: A => G[B])(implicit G: Applicative[G]): G[Identity[B]] =
      G.map(f(ia.run))(Identity(_))
  }

  implicit def idIsTraversable: Traverse[Id] = new Traverse[Id] {
    val foldable: Foldable[Id] = Foldable[Id]
    val functor: Functor[Id] = Functor[Id]
    def traverse[G[_], A, B](a: Id[A])(f: A => G[B])(implicit G: Applicative[G]): G[Id[B]] = f(a)
  }

  private implicit def identityIsApplicative: Applicative[Identity] = new Applicative[Identity] {
    val functor: Functor[Identity] = Functor[Identity]
    def pure[A](a: => A): Identity[A] = Identity(a)
    def apply[A, B](f: Identity[A => B])(fa: Identity[A]): Identity[B] = Identity(f.run(fa.run))
  }

  private implicit def idIsApplicative: Applicative[Id] = new Applicative[Id] {
    val functor: Functor[Id] = Functor[Id]
    def pure[A](a: => A): Id[A] = a
    def apply[A, B](f: Id[A => B])(a: Id[A]): Id[B] = f(a)
  }

  private implicit def identityIsFunctor: Functor[Identity] = new Functor[Identity] {
    def map[A, B](id: Identity[A])(f: A => B): Identity[B] = Identity(f(id.run))
  }

  private implicit def idIsfunctor: Functor[Id] = new Functor[Id] {
    def map[A, B](a: Id[A])(f: A => B): Id[B] = f(a)
  }
}
