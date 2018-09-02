package glue
package data

case class IndexedStateT[F[_], S, T, A](runF: F[S => F[(T, A)]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): IndexedStateT[F, S, T, B] = transform { case (t, a) => (t, f(a)) }

  def bimap[U, B](f: T => U, g: A => B)(implicit F: Functor[F]): IndexedStateT[F, S, U, B] =
    transform { case (t, a) => (f(t), g(a)) }

  def mapF[B](f: A => F[B])(implicit F: Monad[F]): IndexedStateT[F, S, T, B] = IndexedStateT {
    F.map(runF) { sfta =>
      sfta andThen { fta =>
        F.flatMap(fta) { case (t, a) => F.map(f(a))((t, _)) }
      }
    }
  }

  def mapK[G[_]](k: NaturalTransformation[F, G])(implicit F: Functor[F]): IndexedStateT[G, S, T, A] = IndexedStateT {
    k(F.map(runF)(_.andThen(k(_))))
  }

  def mapT[G[_]](f: F[S => F[(T, A)]] => G[S => G[(T, A)]]): IndexedStateT[G, S, T, A] = IndexedStateT(f(runF))

  def flatMap[U, B](f: A => IndexedStateT[F, T, U, B])(implicit F: Monad[F]): IndexedStateT[F, S, U, B] = IndexedStateT {
    F.map(runF) { sfta =>
      sfta andThen { fta =>
        F.flatMap(fta) { case (t, a) => f(a).run(t) }
      }
    }
  }

  def flatMapF[B](f: A => F[B])(implicit F: Monad[F]): IndexedStateT[F, S, T, B] = IndexedStateT {
    F.map(runF) { sfta =>
      sfta andThen { fta =>
        F.flatMap(fta) { case (t, a) => F.map(f(a))((t, _)) }
      }
    }
  }

  def flatMapFF[U, B](f: A => F[T => F[(U, B)]])(implicit F: Monad[F]): IndexedStateT[F, S, U, B] = IndexedStateT {
    F.map(runF) { sfta =>
      sfta andThen { fta =>
        F.flatMap(fta) { case (t, a) => F.flatMap(f(a))(_(t)) }
      }
    }
  }

  def transform[U, B](f: ((T, A)) => (U, B))(implicit F: Functor[F]): IndexedStateT[F, S, U, B] = IndexedStateT {
    F.map(runF) { sfta =>
      sfta andThen { fta =>
        F.map(fta)(f)
      }
    }
  }

  def transformT[G[_]: Applicative, U, B](f: F[(T, A)] => G[(U, B)])(implicit F: Monad[F]): IndexedStateT[G, S, U, B] = IndexedStateT.init {
    s => f(run(s))
  }

  def get(implicit F: Functor[F]): IndexedStateT[F, S, T, T] = inspect(identity)

  def set(t: T)(implicit F: Applicative[F]): IndexedStateT[F, S, T, Unit] = IndexedStateT.init { _ => Applicative[F].pure((t, ())) }

  def put(t: T)(implicit F: Applicative[F]): IndexedStateT[F, S, T, Unit] = put(t)

  def modify[U](f: T => U)(implicit F: Functor[F]): IndexedStateT[F, S, U, A] = transform { case (t, a) => (f(t), a) }

  def inspect[U](f: T => U)(implicit F: Functor[F]): IndexedStateT[F, S, T, U] = transform { case (t, _) => (t, f(t)) }

  def run(s: S)(implicit F: Monad[F]): F[(T, A)] = F.flatMap(runF)(_(s))

  def runT(s: S)(implicit F: Monad[F]): F[T] = F.map(run(s))(_._1)

  def runA(s: S)(implicit F: Monad[F]): F[A] = F.map(run(s))(_._2)

  def runEmpty(implicit F: Monad[F], M: Monoid[S]): F[(T, A)] = run(M.unit)

  def runEmptyT(implicit F: Monad[F], M: Monoid[S]): F[T] = runT(M.unit)

  def runEmptyA(implicit F: Monad[F], M: Monoid[S]): F[A] = runA(M.unit)
}

object IndexedStateT extends IndexedStateTFunctions {
  object implicits extends IndexedStateTImplicits
}

trait IndexedStateTFunctions {
  // Like apply but missing the outer F context
  def init[F[_]: Applicative, S, T, A](f: S => F[(T, A)]): IndexedStateT[F, S, T, A] = IndexedStateT(Applicative[F].pure(f))

  def pure[F[_]: Applicative, S, A](a: A)(implicit d: DummyImplicit): IndexedStateT[F, S, S, A] =
    IndexedStateT(Applicative[F].pure(s => Applicative[F].pure((s, a))))

  def set[F[_]: Applicative, S, T](t: T): IndexedStateT[F, S, T, Unit] = IndexedStateT.init { _ =>
    Applicative[F].pure((t, ()))
  }

  def setF[F[_]: Applicative, S, T](ft: F[T]): IndexedStateT[F, S, T, Unit] = IndexedStateT.init { _ =>
    Applicative[F].map(ft)(t => (t, ()))
  }

  def modify[F[_]: Applicative, S, T](f: S => T): IndexedStateT[F, S, T, Unit] = IndexedStateT.init { s =>
    Applicative[F].pure((f(s), ()))
  }

  def modifyF[F[_]: Applicative, S, T](f: S => F[T]): IndexedStateT[F, S, T, Unit] = IndexedStateT.init { s =>
    Applicative[F].map(f(s))((_, ()))
  }

  def get[F[_]: Applicative, S]: IndexedStateT[F, S, S, S] = IndexedStateT.init {
    s => Applicative[F].pure((s, s))
  }

  def inspect[F[_]: Applicative, S, A](f: S => A): IndexedStateT[F, S, S, A] = IndexedStateT.init {
    s => Applicative[F].pure((s, f(s)))
  }

  def inspectF[F[_]: Applicative, S, A](f: S => F[A]): IndexedStateT[F, S, S, A] = IndexedStateT.init {
    s => Applicative[F].map(f(s))((s, _))
  }

  def liftF[F[_]: Applicative, S, A](fa: F[A]): IndexedStateT[F, S, S, A] = IndexedStateT.init {
    s => Applicative[F].map(fa)((s, _))
  }

  def liftK[F[_]: Applicative, S]: NaturalTransformation[F, ({type f[x] = IndexedStateT[F, S, S, x]})#f] =
    new NaturalTransformation[F, ({type f[x] = IndexedStateT[F, S, S, x]})#f] {
      def apply[A](fa: F[A]): IndexedStateT[F, S, S, A] = liftF(fa)
    }
}

trait IndexedStateTImplicits {
  implicit def indexedStateTIsFunctor[F[_]: Functor, S, T]: Functor[({type f[x] = IndexedStateT[F, S, T, x]})#f] =
    new Functor[({type f[x] = IndexedStateT[F, S, T, x]})#f] {
      def map[A, B](s: IndexedStateT[F, S, T, A])(f: A => B): IndexedStateT[F, S, T, B] = s map f
    }
}

trait StateTFunctions extends IndexedStateTFunctions {
  def constantStateT[F[_]: Applicative, S, A](a: A)(s: => S): StateT[F, S, A] = StateT.init(_ => Applicative[F].pure((s, a)))

  def stateT[F[_]: Applicative, S, A](a: A): StateT[F, S, A] = StateT.init(s => Applicative[F].pure((s, a)))

  // DummyImplicit is needed for avoiding type erasure issues as IndexedStateT.modify would have the same type as this function after erasure.
  def modify[F[_]: Applicative, S](f: S => S)(implicit d: DummyImplicit): StateT[F, S, Unit] = StateT.init(s => Applicative[F].pure((f(s), ())))

  // DummyImplicit is needed for avoiding type erasure issues as IndexedStateT.modifyF would have the same type as this function after erasure.
  def modifyF[F[_]: Applicative, S](f: S => F[S])(implicit d: DummyImplicit): StateT[F, S, Unit] = StateT.init(s => Applicative[F].map(f(s))(s => (s, ())))

  // DummyImplicit is needed for avoiding type erasure issues as IndexedStateT.set would have the same type as this function after erasure.
  def set[F[_]: Applicative, S](s: S)(implicit d: DummyImplicit): StateT[F, S, Unit] = StateT.init(_ => Applicative[F].pure((s, ())))

  // DummyImplicit is needed for avoiding type erasure issues as IndexedStateT.setF would have the same type as this function after erasure.
  def setF[F[_]: Applicative, S](fs: F[S])(implicit d: DummyImplicit): StateT[F, S, Unit] = StateT.init(_ => Applicative[F].map(fs)(s => (s, ())))
}

trait StateTImplicits extends IndexedStateTImplicits
{
  private implicit def stateTIsApplicative[F[_]: Monad: Applicative: Functor, S]: Applicative[({type f[x] = StateT[F, S, x]})#f] =
    new Applicative[({type f[x] = StateT[F, S, x]})#f] {
      val functor: Functor[({type f[x] = StateT[F, S, x]})#f] = Functor[({type f[x] = StateT[F, S, x]})#f]
      def pure[A](a: => A): StateT[F, S, A] = StateT.pure(a)
      def apply[A, B](sf: StateT[F, S, A => B])(sa: StateT[F, S, A]): StateT[F, S, B] = StateT {
        Functor[F].map(sf.runF) { sfsab =>
          sfsab andThen { fsab =>
            Monad[F].flatMap(fsab) { case (s1, f) =>
              Functor[F].map(sa.run(s1)) { case (s2, a) => (s2, f(a)) }
            }
          }
        }
      }
    }

  implicit def indexedStateTIsMonad[F[_]: Monad: Applicative: Functor, S]: Monad[({type f[x] = StateT[F, S, x]})#f] =
    new Monad[({type f[x] = StateT[F, S, x]})#f] {
      val applicative: Applicative[({type f[x] = StateT[F, S, x]})#f] = Applicative[({type f[x] = StateT[F, S, x]})#f]
      def flatMap[A, B](sa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = sa flatMap f
    }
}
