package glue.typeclass

trait Applicative[F[_]] { self =>
  val functor: Functor[F]

  def unit[A](a: => A): F[A]
  def apply[A, B](f: F[A => B])(fa: F[A]): F[B]

  def point[A](a: => A): F[A] = unit(a)
  def pure[A](a: => A): F[A] = unit(a)

  def unit: F[Unit] = unit(())

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def fproduct[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    (List.fill(n)(fa)).foldRight(unit(List[A]()))((fa, l) => map2(fa, l)(_ :: _))

  // The composition of two applicative functors F anf G is a functor of type F[G[α]] for any type α.
  def compose[G[_]](implicit applicativeG: Applicative[G]): Applicative[({type λ[α] = F[G[α]]})#λ] =
    new Applicative[({type λ[α] = F[G[α]]})#λ] {
      val functor: Functor[({type λ[α] = F[G[α]]})#λ] = self.functor compose applicativeG.functor
      def unit[A](a: => A): F[G[A]] = self.unit(applicativeG.unit(a))
      def apply[A, B](fgf: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
        self.map2(fgf, fga) { case (gf, ga) => applicativeG.apply(gf)(ga) }
    }

  // The product of two applicative functors F anf G is a functor of type (F[α], G[α]) for any type α.
  def product[G[_]](implicit applicativeG: Applicative[G]): Applicative[({type λ[α] = (F[α], G[α])})#λ] =
    new Applicative[({type λ[α] = (F[α], G[α])})#λ] {
      val functor: Functor[({type λ[α] = (F[α], G[α])})#λ] = self.functor product applicativeG.functor
      def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), applicativeG.unit(a))
      def apply[A, B](fgf: (F[A => B], G[A => B]))(fga: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fgf._1)(fga._1), applicativeG.apply(fgf._2)(fga._2))
    }
}

object Applicative extends ApplicativeFunctions {
  def apply[F[_]](implicit applicative: Applicative[F]): Applicative[F] = applicative

  object syntax extends ApplicativeSyntax
}

trait ApplicativeFunctions {
  def unit[F[_]: Applicative, A](a: A): F[A] = Applicative[F].unit(a)
  def apply[F[_]: Applicative, A, B](f: F[A => B])(fa: F[A]): F[B] = Applicative[F].apply(f)(fa)

  def map[F[_]: Applicative, A, B](fa: F[A])(f: A => B): F[B] = Applicative[F].map(fa)(f)

  def map2[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    Applicative[F].map2(fa, fb)(f)
  def map3[F[_]: Applicative, A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    Applicative[F].map3(fa, fb, fc)(f)
  def map4[F[_]: Applicative, A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    Applicative[F].map4(fa, fb, fc, fd)(f)

  def fproduct[F[_]: Applicative, A, B](fa: F[A], fb: F[B]): F[(A, B)] = Applicative[F].fproduct(fa, fb)
  def replicateM[F[_]: Applicative, A](n: Int, fa: F[A]): F[List[A]] =
    Applicative[F].replicateM(n, fa)
}

trait ApplicativeSyntax {
  implicit class ApplicativeOps[F[_]: Applicative, A](self: F[A]) {
    def apply[B](f: F[A => B]): F[B] = Applicative[F].apply(f)(self)
    def map[B](f: A => B): F[B] = Applicative[F].map(self)(f)

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = Applicative[F].map2(self, fb)(f)
    def map3[B, C, D](fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = Applicative[F].map3(self, fb, fc)(f)
    def map4[B, C, D, E](fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      Applicative[F].map4(self, fb, fc, fd)(f)

    def fproduct[B](fb: F[B]): F[(A, B)] = Applicative[F].fproduct(self, fb)
    def replicateM(n: Int): F[List[A]] = Applicative[F].replicateM(n, self)
  }
}

trait ApplicativeLaws[F[_]] {
  implicit def applicative: Applicative[F]

  import Applicative._
  import Applicative.syntax._

  def leftIdentity[A](fa: F[A]): Boolean = map2(unit(()), fa)((_, a) => a) == fa
  def rightIdentity[A](fa: F[A]): Boolean = map2(fa, unit(()))((a, _) => a) == fa
  def homomorphism[A, B](a: A, f: A => B): Boolean = unit(a).apply(unit(f)) == unit(f(a))
  def interchange[A, B](a: A, f: F[A => B]): Boolean =
    apply(f)(unit(a)) == apply(unit((f: A => B) => f(a)))(f)
  def composition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): Boolean = {
    val compose: (A => B) => (B => C) => (A => C) = _.andThen
    apply(apply(apply(unit(compose))(fab))(fbc))(fa) == apply(fbc)(fa.apply(fab))
  }
  def applicativeMap[A, B](fa: F[A], f: A => B): Boolean =
    fa.map(f) == fa.apply(unit(f))
  def naturality[A, B, C, D](fa: F[A], fb: F[C], f: A => B, g: C => D): Boolean = {
    def productF(f: A => B, g: C => D): (A, C) => (B, D) = (a, c) => (f(a), g(c))
    map2(fa, fb)(productF(f, g)) == fproduct(map(fa)(f), map(fb)(g))
  }
}

object ApplicativeLaws {
  def apply[F[_]](implicit ev: Applicative[F]): ApplicativeLaws[F] =
    new ApplicativeLaws[F] { def applicative: Applicative[F] = ev }
}
