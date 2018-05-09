package glue
package typeclass

trait Monad[F[_]] { self =>
  val applicative: Applicative[F]

  def unit[A](a: => A): F[A] = applicative.unit(a)

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  // Alias for unit
  def pure[A](a: => A): F[A] = unit(a)

  // Alias for flatMap
  def bind[A, B](ma: F[A])(f: A => F[B]): F[B] = flatMap(ma)(f)

  // Compose two effects discarding the output of the first one
  def seq[A, B](ma: F[A], mb: F[B]): F[B] = flatMap(ma)(Function.const(mb))

  // Functor.map through flatMap and unit
  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  // Applicative.apply and Applicative.map2 through flatMap and map
  def apply[A, B](mab: F[A => B])(ma: F[A]): F[B] = flatMap(mab)(map(ma)(_))
  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List[A]())) { (a, fl) =>
      flatMap(f(a)) { p =>
        if (p) map2(unit(a), fl)(_ :: _)
        else fl
      }
    }

  // Composition of Kleisli arrows, aka 'embellished' functions
  def composeK[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  // Compose two Monads F and G if G (inner Monad) is also Traversable
  def composeM[G[_]](implicit G: Monad[G], T: Traverse[G]): Monad[({type f[x] = F[G[x]]})#f] =
    new Monad[({type f[x] = F[G[x]]})#f] {
      val applicative: Applicative[({type f[x] = F[G[x]]})#f] = self.applicative compose G.applicative
      def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
        self.flatMap(fga)(ga => self.map(T.traverse(ga)(f)(self.applicative))(G.join))
    }

  def as[A, B](fa: F[A])(b: B): F[B] = fa.map(_ => b)

  def skip[A](fa: F[A]): F[Unit] = as(fa)(())

  def doWhile[A](fa: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a <- fa
    p <- cond(a)
    _ <- if (p) doWhile(fa)(cond) else unit(())
  } yield ()

  def forever[A, B](fa: F[A]): F[B] = {
    lazy val t: F[B] = forever(fa)
    fa flatMap (_ => t)
  }

  def foldM[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[B] =
    l.foldLeft(unit(z))((fb, a) => fb.flatMap(b => f(b, a)))

  def foldM_[A, B](l: Stream[A])(z: B)(f: (B, A) => F[B]): F[Unit] =
    skip { foldM(l)(z)(f) }

  def foreachM[A](l: Stream[A])(f: A => F[Unit]): F[Unit] =
    foldM_(l)(()) { (_, a) => skip(f(a)) }

  // This is so we can have for { ... } syntax in trait's functions
  import Monad.syntax._
  implicit def toMonaadic[A](fa: F[A]): MonadOps[F, A] = new MonadOps[F, A](fa)(this)
}

object Monad extends MonadFunctions {
  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  object syntax extends MonadSyntax
}

trait MonadFunctions {
  def unit[F[_]: Monad, A](a: => A): F[A] = Monad[F].unit(a)
  def flatMap[F[_]: Monad, A, B](ma: F[A])(f: A => F[B]): F[B] = Monad[F].flatMap(ma)(f)
  def pure[F[_]: Monad, A](a: => A): F[A] = Monad[F].unit(a)
  def bind[F[_]: Monad, A, B](ma: F[A])(f: A => F[B]): F[B] = Monad[F].bind(ma)(f)
  def seq[F[_]: Monad, A, B](ma: F[A], mb: F[B]): F[B] = Monad[F].seq(ma, mb)
  def map[F[_]: Monad, A, B](ma: F[A])(f: A => B): F[B] = Monad[F].map(ma)(f)
  def apply[F[_]: Monad, A, B](mab: F[A => B])(ma: F[A]): F[B] = Monad[F].apply(mab)(ma)
  def map2[F[_]: Monad, A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = Monad[F].map2(ma, mb)(f)
  def product[F[_]: Monad, A, B](ma: F[A], mb: F[B]): F[(A, B)] = Monad[F].product(ma, mb)
  def filterM[F[_]: Monad, A](as: List[A])(f: A => F[Boolean]): F[List[A]] = Monad[F].filterM(as)(f)
  def composeK[F[_]: Monad, A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = Monad[F].composeK(f, g)
}

trait MonadSyntax {
  implicit class MonadOps[F[_]: Monad, A](self: F[A]) {
    def flatMap[B](f: A => F[B]): F[B] = Monad[F].flatMap(self)(f)
    def bind[B](f: A => F[B]): F[B] = Monad[F].bind(self)(f)
    def seq[B](mb: F[B]): F[B] = Monad[F].seq(self, mb)
    def map[B](f: A => B): F[B] = Monad[F].map(self)(f)
    def apply[B, C](mb: F[B])(implicit ev: A <:< B => C): F[C] = {
      import glue.data.Identity.syntax._
      ev.unused
      Monad[F].apply(self.asInstanceOf[F[B => C]])(mb)
    }
    def map2[B, C](mb: F[B])(f: (A, B) => C): F[C] = Monad[F].map2(self, mb)(f)
    def product[B](mb: F[B]): F[(A, B)] = Monad[F].product(self, mb)
  }
}

trait MonadLaws[F[_]] {
  implicit val monad: Monad[F]

  import Monad.syntax._

  def leftIdentity[A, B](a: A, f: A => F[B]): Boolean =
    monad.flatMap(monad.unit(a))(f) == f(a) // compose(unit, f) == f
  def rightIdentity[A](fa: F[A]): Boolean =
    monad.flatMap(fa)(monad.unit(_)) == fa  // compose(f, unit) == f
  def associativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): Boolean =
    fa.flatMap(f).flatMap(g) == fa.flatMap(a => f(a).flatMap(g))  // compose(compose(f, g), h) == compose(f, compose(g, h))
  def coherence[A, B](fa: F[A], f: A => B): Boolean =
    fa.flatMap(a => monad.unit(f(a))) == fa.map(f)
}

object MonadLaws {
  def apply[F[_]](implicit F: Monad[F]): MonadLaws[F] =
    new MonadLaws[F] { val monad: Monad[F] = F }
}
