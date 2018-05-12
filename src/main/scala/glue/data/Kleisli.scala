package glue
package data

final case class Kleisli[F[_], A, B](run: A => F[B]) {
  def map[C](f: B => C)(implicit F: Functor[F]): Kleisli[F, A, C] = Kleisli { a =>
    F.map(run(a))(b => f(b))
  }

  def mapT[G[_], C](t: F[B] => F[C]): Kleisli[F, A, C] = Kleisli(t compose run)

  def mapF[C](f: B => F[C])(implicit G: Monad[F]): Kleisli[F, A, C] = Kleisli(a => G.flatMap(run(a))(f))

  def mapK[G[_]](k: NaturalTransformation[F, G]): Kleisli[G, A, B] = Kleisli(run andThen k.apply)

  def apply[C](ff: Kleisli[F, A, B => C])(implicit G: Applicative[F]): Kleisli[F, A, C] = Kleisli { a =>
    G.apply(ff.run(a))(run(a))
  }

  def flatMap[C](f: B => Kleisli[F, A, C])(implicit G: Monad[F]): Kleisli[F, A, C] = Kleisli { a =>
    G.flatMap(run(a))(b => f(b).run(a))
  }

  def flatMapF[C](f: B => F[C])(implicit G: Monad[F]): Kleisli[F, A, C] = Kleisli { a =>
    G.flatMap(run(a))(f)
  }

  def andThen[C](f: B => F[C])(implicit G: Monad[F]): Kleisli[F, A, C] = mapF(f)

  def andThen[C](k: Kleisli[F, B, C])(implicit G: Monad[F]): Kleisli[F, A, C] = mapF(k.run)

  def compose[Z](f: Z => F[A])(implicit G: Monad[F]): Kleisli[F, Z, B] = Kleisli(z => G.flatMap(f(z))(run))

  def compose[Z](k: Kleisli[F, Z, A])(implicit G: Monad[F]): Kleisli[F, Z, B] = k andThen run

  def traverse[G[_]: Traverse](ga: G[A])(implicit F: Applicative[F]): F[G[B]] = Traverse[G].traverse(ga)(run)

  def lift[G[_]: Applicative]: Kleisli[({type f[x] = G[F[x]]})#f, A, B] =
    Kleisli[({type f[x] = G[F[x]]})#f, A, B](a => Applicative[G].pure(run(a)))

  def apply(a: A): F[B] = run(a)
}

object Kleisli extends KleisliFunctions {
  object implicits extends KleisliImplicits
}

trait KleisliFunctions {
  def liftK[F[_], Z]: NaturalTransformation[F, ({type f[x] = Kleisli[F, Z, x]})#f] =
    new NaturalTransformation[F, ({type f[x] = Kleisli[F, Z, x]})#f] {
      def apply[A](fa: F[A]): Kleisli[F, Z, A] = liftF(fa)
    }

  def liftA[F[_]: Applicative, A, B](f: A => B): Kleisli[F, A, B] = Kleisli(a => Applicative[F].pure(f(a)))
  def lift[F[_]: Applicative, A, B](f: A => B): Kleisli[F, A, B] = liftA(f)

  def liftF[F[_], A, B](fb: F[B]): Kleisli[F, A, B] = Kleisli(_ => fb)

  def compose[F[_]: Monad, A, B, C](k1: Kleisli[F, B, C], k2: Kleisli[F, A, B]): Kleisli[F, A, C] =
    k1 compose k2

  def andThen[F[_]: Monad, A, B, C](k1: Kleisli[F, A, B], k2: Kleisli[F, B, C]): Kleisli[F, A, C] =
    k1 andThen k2

  def pure[F[_]: Applicative, A, B](b: B): Kleisli[F, A, B] = Kleisli(_ => Applicative[F].pure(b))

  def traverse[F[_]: Applicative, G[_]: Traverse, A, B](ga: G[A])(k: Kleisli[F, A, B]): F[G[B]] =
    Traverse[G].traverse(ga)(k.run)

  def read[F[_]: Applicative, A]: Kleisli[F, A, A] = Kleisli(Applicative[F].pure(_))

  def identity[F[_]: Applicative, A]: Kleisli[F, A, A] = read
}

trait KleisliImplicits {
  private implicit def kleisliIsFunctor[F[_]: Functor, Z]: Functor[({type f[x] = Kleisli[F, Z, x]})#f] =
    new Functor[({type f[x] = Kleisli[F, Z, x]})#f] {
      def map[A, B](k: Kleisli[F, Z, A])(f: A => B): Kleisli[F, Z, B] = k map f
    }

  private implicit def kleisliIsApplicative[F[_]: Applicative: Functor, Z]: Applicative[({type f[x] = Kleisli[F, Z, x]})#f] =
    new Applicative[({type f[x] = Kleisli[F, Z, x]})#f] {
      val functor: Functor[({type f[x] = Kleisli[F, Z, x]})#f] = Functor[({type f[x] = Kleisli[F, Z, x]})#f]
      def pure[A](a: => A): Kleisli[F, Z, A] = pure(a)
      def apply[A, B](kf: Kleisli[F, Z, A => B])(ka: Kleisli[F, Z, A]): Kleisli[F, Z, B] =
        ka apply kf
    }

  implicit def kleisliIsMonad[F[_]: Monad: Applicative: Functor, Z]: Monad[({type f[x] = Kleisli[F, Z, x]})#f] =
    new Monad[({type f[x] = Kleisli[F, Z, x]})#f] {
      val applicative: Applicative[({type f[x] = Kleisli[F, Z, x]})#f] = Applicative[({type f[x] = Kleisli[F, Z, x]})#f]
      def flatMap[A, B](k: Kleisli[F, Z, A])(f: A => Kleisli[F, Z, B]): Kleisli[F, Z, B] =
        k flatMap f
    }
}
