package glue.typeclass

trait Functor[F[_]] { self =>
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def fmap[A, B](f: A => B, fa: F[A]): F[B] = map(fa)(f)
  def replaceL[A, B](a: A, fb: F[B]): F[A] = map(fb) { _ => a }
  def replaceR[A, B](fa: F[A], b: B): F[B] = map(fa) { _ => b }
  def void[A](fa: F[A]): F[Unit] = map(fa) { _ => () }
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  def apply[A, B](ff: F[A => B], a: A): F[B] = map(ff)(f => f(a))
  def fcompose[A, B, C](fa: F[A])(f: A => B, g: B => C): F[C] = map(fa)(f andThen g)
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
  def pair[A](fa: F[A]): F[(A, A)] = map(fa) { a => (a, a) }
  def fpair[A, B](fa: F[A])(f: A => B): F[(A, B)] = map(fa) { a => (a, f(a)) }
  def strengthL[A, B](a: A, fb: F[B]): F[(A, B)] = map(fb) { b => (a, b) }
  def strengthR[A, B](fa: F[A], b: B): F[(A, B)] = map(fa) { a => (a, b) }

  // The composition of two functors F anf G is a functor of type F[G[x]] for any type x.
  def compose[G[_]](implicit G: Functor[G]): Functor[({type f[x] = F[G[x]]})#f] =
    new Functor[({type f[x] = F[G[x]]})#f] {
      def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = self.map(fga)(G.lift(f))
    }

  // The product of two functors F anf G is a functor of type (F[x], G[x]) for any type x.
  def product[G[_]](implicit G: Functor[G]): Functor[({type f[x] = (F[x], G[x])})#f] =
    new Functor[({type f[x] = (F[x], G[x])})#f] {
      def map[A, B](fa: (F[A], G[A]))(f: A => B): (F[B], G[B]) = (self.map(fa._1)(f), G.map(fa._2)(f))
    }
}

object Functor extends FunctorFunctions {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  object syntax extends FunctorSyntax
}

trait FunctorFunctions {
  def map[F[_]: Functor, A, B](fa: F[A])(f: A => B): F[B] = Functor[F].map(fa)(f)
  def fmap[F[_]: Functor, A, B](f: A => B, fa: F[A]): F[B] = Functor[F].fmap(f, fa)
  def replaceL[F[_]: Functor, A, B](a: A, fb: F[B]): F[A] = Functor[F].replaceL(a, fb)
  def replaceR[F[_]: Functor, A, B](fa: F[A], b: B): F[B] = Functor[F].replaceR(fa, b)
  def void[F[_]: Functor, A](fa: F[A]): F[Unit] = Functor[F].void(fa)
  def lift[F[_]: Functor, A, B](f: A => B): F[A] => F[B] = Functor[F].lift(f)
  def apply[F[_]: Functor, A, B](ff: F[A => B], a: A): F[B] = Functor[F].apply(ff, a)
  def fcompose[F[_]: Functor, A, B, C](fa: F[A])(f: A => B, g: B => C): F[C] = Functor[F].fcompose(fa)(f, g)
  def distribute[F[_]: Functor, A, B](fab: F[(A, B)]): (F[A], F[B]) = Functor[F].distribute(fab)
  def codistribute[F[_]: Functor, A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = Functor[F].codistribute(e)
  def pair[F[_]: Functor, A](fa: F[A]): F[(A, A)] = Functor[F].pair(fa)
  def fpair[F[_]: Functor, A, B](fa: F[A])(f: A => B): F[(A, B)] = Functor[F].fpair(fa)(f)
  def strengthL[F[_]: Functor, A, B](a: A, fb: F[B]): F[(A, B)] = Functor[F].strengthL(a, fb)
  def strengthR[F[_]: Functor, A, B](fa: F[A], b: B): F[(A, B)] = Functor[F].strengthR(fa, b)
}

trait FunctorSyntax {
  implicit class FunctorOps[F[_]: Functor, A](self: F[A]) {
    def map[B](f: A => B): F[B] = Functor[F].map(self)(f)
    def fmap[B](f: A => B): F[B] = Functor[F].fmap(f, self)
    def replaceL[B](b: B): F[B] = Functor[F].replaceL(b, self)
    def replaceR[B](b: B): F[B] = Functor[F].replaceR(self, b)
    def as[B](b: B): F[B] = Functor[F].replaceR(self, b)
    def void: F[Unit] = Functor[F].void(self)
    def fcompose[B, C](f: A => B, g: B => C): F[C] = Functor[F].fcompose(self)(f, g)
    def pair: F[(A, A)] = Functor[F].pair(self)
    def fpair[B](f: A => B): F[(A, B)] = Functor[F].fpair(self)(f)
    def strengthL[B](b: B): F[(B, A)] = Functor[F].strengthL(b, self)
    def strengthR[B](b: B): F[(A, B)] = Functor[F].strengthR(self, b)
  }
}

trait FunctorLaws[F[_]] {
  implicit def functor: Functor[F]

  import Functor.syntax._

  def identity[A](fa: F[A]): Boolean = fa.map(Predef.identity) == fa
  def composition[A, B, C](fa: F[A], f: A => B, g: B => C): Boolean =
    fa.map(f).map(g) == fa.map(f andThen g)
}

object FunctorLaws {
  def apply[F[_]](implicit F: Functor[F]): FunctorLaws[F] =
    new FunctorLaws[F] { def functor: Functor[F] = F }
}
