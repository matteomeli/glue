package glue
package data

import glue.typeclass.{Applicative, Foldable, Functor, Monad, Monoid, Traverse}

final case class Identity[A](run: A) extends AnyVal

object Identity {
  object implicits extends IdentityImplicits
}

trait IdentityImplicits {
  private implicit val identityIsFunctor: Functor[Identity] = new Functor[Identity] {
    def map[A, B](id: Identity[A])(f: A => B): Identity[B] = Identity(f(id.run))
  }

  private implicit val identityIsApplicative: Applicative[Identity] = new Applicative[Identity] {
    val functor: Functor[Identity] = Functor[Identity]
    def pure[A](a: => A): Identity[A] = Identity(a)
    def apply[A, B](f: Identity[A => B])(fa: Identity[A]): Identity[B] = Identity(f.run(fa.run))
  }

  implicit val identityIsIsMonad: Monad[Identity] = new Monad[Identity] {
    val applicative: Applicative[Identity] = Applicative[Identity]
    def flatMap[A, B](ia: Identity[A])(f: A => Identity[B]): Identity[B] = f(ia.run)
  }

  private implicit val identityIsFoldable: Foldable[Identity] = new Foldable[Identity] {
    def foldLeft[A, B](ia: Identity[A], z: B)(f: (B, A) => B): B = f(z, ia.run)
    def foldRight[A, B](ia: Identity[A], z: B)(f: (A, B) => B): B = f(ia.run, z)
    def foldMap[A, B](ia: Identity[A])(f: A => B)(implicit M: Monoid[B]): B = M.combine(M.unit, f(ia.run))
  }

  implicit def identityIsTraversable: Traverse[Identity] = new Traverse[Identity] {
    val foldable: Foldable[Identity] = Foldable[Identity]
    val functor: Functor[Identity] = Functor[Identity]
    def traverse[G[_], A, B](ia: Identity[A])(f: A => G[B])(implicit G: Applicative[G]): G[Identity[B]] =
      G.map(f(ia.run))(Identity(_))
  }
}
