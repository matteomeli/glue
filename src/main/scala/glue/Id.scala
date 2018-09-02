package glue

trait IdImplicits {
  type Id[A] = A
  
  implicit class IdOps[A](a: A) {
    def unused(): Unit = ()
  }

  implicit val idIsfunctor: Functor[Id] = new Functor[Id] {
    def map[A, B](a: Id[A])(f: A => B): Id[B] = f(a)
  }
  implicit val idIsApplicative: Applicative[Id] = new Applicative[Id] {
    val functor: Functor[Id] = Functor[Id]
    def pure[A](a: => A): Id[A] = a
    def apply[A, B](f: Id[A => B])(a: Id[A]): Id[B] = f(a)
  }
  implicit val idIsMonad: Monad[Id] = new Monad[Id] {
    val applicative: Applicative[Id] = Applicative[Id]
    def flatMap[A, B](a: Id[A])(f: A => Id[B]): Id[B] = f(a)
  }
  implicit val idIsFoldable: Foldable[Id] = new Foldable[Id] {
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

object Id extends IdImplicits