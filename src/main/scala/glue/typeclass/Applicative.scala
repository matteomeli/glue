package glue
package typeclass

trait Applicative[F[_]] { self =>
  val functor: Functor[F]

  def pure[A](a: => A): F[A]
  def apply[A, B](f: F[A => B])(fa: F[A]): F[B]

  // Synonym for pure
  def point[A](a: => A): F[A] = pure(a)

  def unit: F[Unit] = pure(())

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(pure(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    apply(apply(pure(f.curried))(fa))(fb)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    apply(apply(apply(pure(f.curried))(fa))(fb))(fc)
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    apply(apply(apply(apply(pure(f.curried))(fa))(fb))(fc))(fd)

  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))

  // The composition of two applicative functors F anf G is a functor of type F[G[x]] for any type x.
  def compose[G[_]](implicit G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      val functor: Functor[({type f[x] = F[G[x]]})#f] = self.functor compose G.functor
      def pure[A](a: => A): F[G[A]] = self.pure(G.pure(a))
      def apply[A, B](fgf: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
        self.map2(fgf, fga) { case (gf, ga) => G.apply(gf)(ga) }
    }

  // The product of two applicative functors F anf G is a functor of type (F[x], G[x]) for any type x.
  def product[G[_]](implicit G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      val functor: Functor[({type f[x] = (F[x], G[x])})#f] = self.functor product G.functor
      def pure[A](a: => A): (F[A], G[A]) = (self.pure(a), G.pure(a))
      def apply[A, B](fgf: (F[A => B], G[A => B]))(fga: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fgf._1)(fga._1), G.apply(fgf._2)(fga._2))
    }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    (List.fill(n)(fa)).foldRight(pure(List[A]()))((fa, l) => map2(fa, l)(_ :: _))
  def seqRight[A, B](fa: F[A], fb: F[B]): F[B] = apply(functor.left(identity[B] _, fa))(fb)
  def seqLeft[A, B](fa: F[A], fb: F[B]): F[A] = map2(fa, fb)((a, b) => Function.const(a)(b))
}

object Applicative extends ApplicativeFunctions {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  object syntax extends ApplicativeSyntax
}

trait ApplicativeFunctions {
  def pure[F[_]: Applicative, A](a: A): F[A] = Applicative[F].pure(a)
  def apply[F[_]: Applicative, A, B](f: F[A => B])(fa: F[A]): F[B] = Applicative[F].apply(f)(fa)

  def map[F[_]: Applicative, A, B](fa: F[A])(f: A => B): F[B] = Applicative[F].map(fa)(f)

  def map2[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    Applicative[F].map2(fa, fb)(f)
  def map3[F[_]: Applicative, A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] =
    Applicative[F].map3(fa, fb, fc)(f)
  def map4[F[_]: Applicative, A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
    Applicative[F].map4(fa, fb, fc, fd)(f)

  def product[F[_]: Applicative, A, B](fa: F[A], fb: F[B]): F[(A, B)] = Applicative[F].product(fa, fb)

  def replicateM[F[_]: Applicative, A](n: Int, fa: F[A]): F[List[A]] =
    Applicative[F].replicateM(n, fa)
  def seqRight[F[_]: Applicative, A, B](fa: F[A], fb: F[B]): F[B] = Applicative[F].seqRight(fa, fb)
  def seqLeft[F[_]: Applicative, A, B](fa: F[A], fb: F[B]): F[A] = Applicative[F].seqLeft(fa, fb)
}

trait ApplicativeSyntax {
  implicit class ApplicativeOps[F[_]: Applicative, A](self: F[A]) {
    def apply[B](f: F[A => B]): F[B] = Applicative[F].apply(f)(self)

    // Allow the apply to be injected as well when A is of type Function1[B, C]
    def apply[B, C](fb: F[B])(implicit ev: A <:< B => C): F[C] = {
      // This function uses generalized type constraints as implicit evidence parameters
      // If -Ywarn-unused:implicits is passed to the compiler,
      // it will issue a warning about ev being unused.
      // If -Xfatal-warnings is used, compilation will fail.
      // The following line allows compilation under those two flags.
      ev.unused
      Applicative[F].apply(self.asInstanceOf[F[B => C]])(fb)
    }

    def map[B](f: A => B): F[B] = Applicative[F].map(self)(f)

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = Applicative[F].map2(self, fb)(f)
    def map3[B, C, D](fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = Applicative[F].map3(self, fb, fc)(f)
    def map4[B, C, D, E](fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] =
      Applicative[F].map4(self, fb, fc, fd)(f)

    def product[B](fb: F[B]): F[(A, B)] = Applicative[F].product(self, fb)
    def replicateM(n: Int): F[List[A]] = Applicative[F].replicateM(n, self)

    def seqRight[B](fb: F[B]): F[B] = Applicative[F].seqRight(self, fb)
  }
}

trait ApplicativeLaws[F[_]] {
  implicit val applicative: Applicative[F]

  import Applicative._
  import Applicative.syntax._

  def leftIdentity[A](fa: F[A]): Boolean = map2(pure(()), fa)((_, a) => a) == fa
  def rightIdentity[A](fa: F[A]): Boolean = map2(fa, pure(()))((a, _) => a) == fa
  def associativity[A, B, C](fa: F[A], fb: F[B], fc: F[C]): Boolean = {
    def reassoc(p: (A, (B, C))): ((A, B), C) = p match { case (a, (b, c)) => ((a, b), c)}
    product(product(fa, fb), fc) == map(product(fa, product(fb, fc)))(reassoc)
  }
  def homomorphism[A, B](a: A, f: A => B): Boolean = pure(a).apply(pure(f)) == pure(f(a))
  def interchange[A, B](a: A, ff: F[A => B]): Boolean =
    apply(ff)(pure(a)) == apply(pure((f: A => B) => f(a)))(ff)
  def composition[A, B, C](fa: F[A], fab: F[A => B], fbc: F[B => C]): Boolean = {
    val compose: (A => B) => (B => C) => (A => C) = _.andThen
    pure(compose).apply(fab).apply(fbc).apply(fa) == fbc.apply(fab.apply(fa))
  }
  def applicativeMap[A, B](fa: F[A], f: A => B): Boolean =
    fa.map(f) == fa.apply(pure(f))
  def naturality[A, B, C, D](fa: F[A], fb: F[C], f: A => B, g: C => D): Boolean = {
    def pair(f: A => B, g: C => D): (A, C) => (B, D) = (a, c) => (f(a), g(c))
    map2(fa, fb)(pair(f, g)) == product(map(fa)(f), map(fb)(g))
  }
}

object ApplicativeLaws {
  def apply[F[_]](implicit F: Applicative[F]): ApplicativeLaws[F] =
    new ApplicativeLaws[F] { val applicative: Applicative[F] = F }
}
