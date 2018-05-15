package glue
package data

import glue.std.either
import glue.std.either._
import glue.std.option._

case class EitherT[F[_], A, B](run: F[Either[A, B]]) {
  def bimap[C, D](f: A => C, g: B => D)(implicit F: Functor[F]): EitherT[F, C, D] = EitherT {
    F.map(run) {
      case Left(a) => Left(f(a))
      case Right(b) => Right(g(b))
    }
  }

  def map[D](f: B => D)(implicit F: Functor[F]): EitherT[F, A, D] = bimap(identity, f)

  def mapT[G[_]](t: F[Either[A, B]] => G[Either[A, B]]): EitherT[G, A, B] = EitherT(t(run))

  def mapK[G[_]](k: NaturalTransformation[F, G]): EitherT[G, A, B] = EitherT(k(run))

  def mapF[D](f: B => F[D])(implicit F: Monad[F]): EitherT[F, A, D] = EitherT {
    F.flatMap(run) {
      case Left(a) => F.unit(Left(a))
      case Right(b) => F.map(f(b))(Right(_))
    }
  }

  def flatMap[D](f: B => EitherT[F, A, D])(implicit F: Monad[F]): EitherT[F, A, D] = EitherT {
    F.flatMap(run) {
      case Left(a) => F.unit(Left(a))
      case Right(b) => f(b).run
    }
  }

  def flatMapF[D](f: B => F[Either[A, D]])(implicit F: Monad[F]): EitherT[F, A, D] = EitherT {
    F.flatMap(run) {
      case Left(a) => F.unit(Left(a))
      case Right(b) => f(b)
    }
  }

  def apply[D](ef: => EitherT[F, A, B => D])(implicit F: Applicative[F]): EitherT[F, A, D] = EitherT {
    F.map2(ef.run, run) { (ef, eb) =>
      eitherIsMonad[A].applicative.apply(ef)(eb)
    }
  }

  def map2[D, E](e: EitherT[F, A, D])(f: (B, D) => E)(implicit F: Applicative[F]): EitherT[F, A, E] = EitherT {
    F.map2(run, e.run)((eb, ec) => eitherIsMonad[A].applicative.map2(eb, ec)(f))
  }

  def foldLeft[D](z: D)(f: (D, B) => D)(implicit F: Foldable[F]): D =
    F.compose(eitherIsTraversable[A].foldable).foldLeft(run, z)(f)

  def foldRight[D](z: D)(f: (B, D) => D)(implicit F: Foldable[F]): D =
    F.compose(eitherIsTraversable[A].foldable).foldRight(run, z)(f)

  def foldMap[D](f: B => D)(implicit F: Foldable[F], M: Monoid[D]): D =
    F.compose(eitherIsTraversable[A].foldable).foldMap(run)(f)

  def traverse[G[_]: Applicative, D](f: B => G[D])(implicit T: Traverse[F]): G[EitherT[F, A, D]] =
    Applicative[G].map(T.compose(eitherIsTraversable[A]).traverse(run)(f))(EitherT(_))

  def leftMap[C](f: A => C)(implicit F: Functor[F]): EitherT[F, C, B] = bimap(f, identity)

  def leftFlatmap[C](f: A => EitherT[F, C, B])(implicit F: Monad[F]): EitherT[F, C, B] = EitherT {
    F.flatMap(run) {
      case Left(a) => f(a).run
      case Right(b) => F.unit(Right(b))
    }
  }

  def leftFlatmapF[C](f: A => F[Either[C, B]])(implicit F: Monad[F]): EitherT[F, C, B] = EitherT {
    F.flatMap(run) {
      case Left(a) => f(a)
      case Right(b) => F.unit(Right(b))
    }
  }

  def isLeft(implicit F: Functor[F]): F[Boolean] = F.map(run)(_.isLeft)

  def isRight(implicit F: Functor[F]): F[Boolean] = F.map(run)(_.isRight)

  def exists(p: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(run)(_.exists(p))

  def filterOrElse[A1 >: A](p: B => Boolean, zero: => A1)(implicit F: Functor[F]): EitherT[F, A1, B] = EitherT {
    F.map(run)(_.filterOrElse(p, zero))
  }

  def fold[C](f: A => C, g: B => C)(implicit F: Functor[F]): F[C] = F.map(run)(_.fold(f, g))

  def forall(p: B => Boolean)(implicit F: Functor[F]): F[Boolean] = F.map(run)(_.forall(p))

  def getOrElse[B1 >: B](z: => B1)(implicit F: Functor[F]): F[B1] = F.map(run)(_.getOrElse(z))

  def getOrElseF(z: => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(run) {
    case Left(_) => z
    case Right(b) => F.unit(b)
  }

  def orElse(z: => EitherT[F, A, B])(implicit F: Monad[F]): EitherT[F, A, B] = EitherT {
    F.flatMap(run) {
      case Left(_) => z.run
      case r @ Right(_) => F.unit(r)
    }
  }

  def orElseF(z: => F[Either[A, B]])(implicit F: Monad[F]): EitherT[F, A, B] = EitherT {
    F.flatMap(run) {
      case Left(_) => z
      case r @ Right(_) => F.unit(r)
    }
  }

  def swap(implicit F: Functor[F]): EitherT[F, B, A] = EitherT {
    F.map(run) {
      case Left(a) => Right(a)
      case Right(b) => Left(b)
    }
  }

  def toOption(implicit F: Functor[F]): OptionT[F, B] = OptionT {
    F.map(run) {
      case Left(_) => none[B]
      case Right(b) => some(b)
    }
  }
}

object EitherT extends EitherTFunctions {
  object implicits extends EitherTImplicits
}

trait EitherTFunctions {
  def pure[F[_]: Applicative, A, B](b: B): EitherT[F, A, B] = EitherT(Applicative[F].pure(Right(b)))

  def left[F[_]: Functor, A, B](fa: F[A]): EitherT[F, A, B] = EitherT(Functor[F].map(fa)(Left(_)))
  def leftT[F[_]: Applicative, A, B](a: A): EitherT[F, A, B] = EitherT(Applicative[F].pure(Left(a)))

  def right[F[_]: Functor, A, B](fb: F[B]): EitherT[F, A, B] = EitherT(Functor[F].map(fb)(Right(_)))
  def rightT[F[_]: Applicative, A, B](b: B): EitherT[F, A, B] = pure(b)

  def liftF[F[_]: Functor, A, B](fb: F[B]): EitherT[F, A, B] = right(fb)

  // DummyImplicit is needed for avoiding type erasure issues as OptionT.liftK would have the same type as this function after erasure.
  def liftK[F[_]: Functor, L](implicit d: DummyImplicit): NaturalTransformation[F, ({type f[x] = EitherT[F, L, x]})#f] =
    new NaturalTransformation[F, ({type f[x] = EitherT[F, L, x]})#f] {
      def apply[A](fa: F[A]): EitherT[F, L, A] = right(fa)
    }

  def fromEither[F[_]: Applicative, A, B](e: Either[A, B]): EitherT[F, A, B] = EitherT(Applicative[F].pure(e))

  def fromOption[F[_]: Applicative, L, A](o: Option[A], z: => L): EitherT[F, L, A] = EitherT(Applicative[F].pure(either.fromOption(o, z)))
  def fromOptionF[F[_]: Functor, L, A](fo: F[Option[A]], z: => L): EitherT[F, L, A] = EitherT(Functor[F].map(fo)(either.fromOption(_, z)))
}

trait EitherTImplicits {
  private implicit def eitherTisFunctor[F[_]: Functor, L]: Functor[({type f[x] = EitherT[F, L, x]})#f] =
    new Functor[({type f[x] = EitherT[F, L, x]})#f] {
      def map[A, B](e: EitherT[F, L, A])(f: A => B): EitherT[F, L, B] = e map f
    }

  private implicit def eitherTisApplicative[F[_]: Applicative: Functor, L]: Applicative[({type f[x] = EitherT[F, L, x]})#f] =
    new Applicative[({type f[x] = EitherT[F, L, x]})#f] {
      val functor: Functor[({type f[x] = EitherT[F, L, x]})#f] = Functor[({type f[x] = EitherT[F, L, x]})#f]
      def pure[A](a: => A): EitherT[F, L, A] = EitherT.pure(a)
      def apply[A, B](ef: EitherT[F, L, A => B])(ea: EitherT[F, L, A]): EitherT[F, L, B] = ea apply ef
    }

  implicit def eitherTisMonad[F[_]: Monad: Applicative: Functor, L]: Monad[({type f[x] = EitherT[F, L, x]})#f] =
    new Monad[({type f[x] = EitherT[F, L, x]})#f] {
      val applicative: Applicative[({type f[x] = EitherT[F, L, x]})#f] = Applicative[({type f[x] = EitherT[F, L, x]})#f]
      def flatMap[A, B](e: EitherT[F, L, A])(f: A => EitherT[F, L, B]): EitherT[F, L, B] = e flatMap f
    }

  private implicit def eitherTisFoldable[F[_]: Foldable, L]: Foldable[({type f[x] = EitherT[F, L, x]})#f] =
    new Foldable[({type f[x] = EitherT[F, L, x]})#f] {
      def foldLeft[A, B](e: EitherT[F, L, A], z: B)(f: (B, A) => B): B = e.foldLeft(z)(f)
      def foldRight[A, B](e: EitherT[F, L, A], z: B)(f: (A, B) => B): B = e.foldRight(z)(f)
      def foldMap[A, B](e: EitherT[F, L, A])(f: A => B)(implicit M: Monoid[B]): B = e foldMap f
    }

  implicit def eitherTisTraversable[F[_]: Traverse: Functor: Foldable, L]: Traverse[({type f[x] = EitherT[F, L, x]})#f] =
    new Traverse[({type f[x] = EitherT[F, L, x]})#f] {
      val foldable: Foldable[({type f[x] = EitherT[F, L, x]})#f] = Foldable[({type f[x] = EitherT[F, L, x]})#f]
      val functor: Functor[({type f[x] = EitherT[F, L, x]})#f] = Functor[({type f[x] = EitherT[F, L, x]})#f]
      def traverse[G[_], A, B](fa: EitherT[F, L, A])(f: A => G[B])(implicit G: Applicative[G]): G[EitherT[F, L, B]] =
        fa traverse f
    }
}
