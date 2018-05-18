package glue
package data

case class WriterT[F[_], W, A](run: F[(W, A)]) {
  def map[B](f: A => B)(implicit F: Functor[F]): WriterT[F, W, B] = WriterT {
    F.map(run) { case (w, a) => (w, f(a)) }
  }

  def mapF[B](f: A => F[B])(implicit F: Monad[F]): WriterT[F, W, B] = WriterT {
    F.flatMap(run) { case (w, a) => F.map(f(a))((w, _)) }
  }

  def mapT[G[_]](t: F[(W, A)] => G[(W, A)]): WriterT[G, W, A] = WriterT(t(run))

  def mapK[G[_]](k: NaturalTransformation[F, G]): WriterT[G, W, A] = WriterT(k(run))

  def flatMap[B](f: A => WriterT[F, W, B])(implicit F: Monad[F], M: Monoid[W]): WriterT[F, W, B] = WriterT {
    F.flatMap(run) {
      case (w1, a) =>  F.map(f(a).run) {
        case (w2, b) => (M.combine(w1, w2), b)
      }
    }
  }

  def flatMapF[B](f: A => F[(W, B)])(implicit F: Monad[F], M: Monoid[W]): WriterT[F, W, B] = WriterT {
    F.flatMap(run) {
      case (w1, a) => F.map(f(a)) {
        case (w2, b) => (M.combine(w1, w2), b)
      }
    }
  }

  def apply[B](wf: WriterT[F, W, A => B])(implicit F: Applicative[F], M: Monoid[W]): WriterT[F, W, B] = WriterT {
    F.map2(wf.run, run) { case ((w1, f), (w2, a)) => (M.combine(w1, w2), f(a)) }
  }

  def map2[B, C](w: WriterT[F, W, B])(f: (A, B) => C)(implicit F: Applicative[F], M: Monoid[W]): WriterT[F, W, C] = WriterT {
    F.map2(run, w.run) { case ((w1, a), (w2, b)) => (M.combine(w1, w2), f(a, b)) }
  }

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.foldLeft(run, z) { case (b, (_, a)) => f(b, a) }

  def foldRight[B](z: B)(f: (A, B) => B)(implicit F: Foldable[F]): B =
    F.foldRight(run, z) { case ((_, a), b) => f(a, b) }

  def foldMap[B](f: A => B)(implicit F: Foldable[F], M: Monoid[B]): B =
    F.foldMap(run) { case (_, a) => f(a) }

  def traverse[G[_], B](f: A => G[B])(implicit T: Traverse[F], G: Applicative[G]): G[WriterT[F, W, B]] =
    G.map(T.traverse(run) { case (w, a) => G.map(f(a))((w, _)) })(WriterT(_))

  def bimap[X, B](f: W => X, g: A => B)(implicit F: Functor[F]): WriterT[F, X, B] = WriterT {
    F.map(run) { case (w, a) => (f(w), g(a)) }
  }

  def transform[X, B](f: ((W, A)) => (X, B))(implicit F: Functor[F]): WriterT[F, X, B] = WriterT {
    F.map(run)(f)
  }

  def leftMap[X](f: W => X)(implicit F: Functor[F]): WriterT[F, X, A] = bimap(f, identity)

  def written(implicit F: Functor[F]): F[W] = F.map(run)(_._1)

  def value(implicit F: Functor[F]): F[A] = F.map(run)(_._2)

  def reset(implicit F: Functor[F], M: Monoid[W]): WriterT[F, W, A] = WriterT {
    F.map(run) { case (_, a) => (M.unit, a) }
  }

  def swap(implicit F: Functor[F]): WriterT[F, A, W] = WriterT {
    F.map(run) { case (w, a) => (a, w) }
  }
}

object WriterT extends WriterTFunctions {
  def apply[F[_]: Applicative, W, A](w: W, a: A): WriterT[F, W, A] = writerT((w, a))

  object implicits extends WriterTImplicits
}

trait WriterTFunctions {
  def pure[F[_]: Applicative, W: Monoid, A](a: A): WriterT[F, W, A] = WriterT(Applicative[F].pure((Monoid[W].unit, a)))

  def writerT[F[_]: Applicative, W, A](wa: (W, A)): WriterT[F, W, A] = WriterT(Applicative[F].pure(wa))

  def liftF[F[_]: Functor, W: Monoid, A](fa: F[A]): WriterT[F, W, A] = WriterT(Functor[F].map(fa)((Monoid[W].unit, _)))

  def liftK[F[_]: Functor, W: Monoid]: NaturalTransformation[F, ({type f[x] = WriterT[F, W, x]})#f] =
    new NaturalTransformation[F, ({type f[x] = WriterT[F, W, x]})#f] {
      def apply[A](fa: F[A]): WriterT[F, W, A] = WriterT(Functor[F].map(fa)((Monoid[W].unit, _)))
    }

  def put[F[_]: Applicative, W, A](w: W, a: A): WriterT[F, W, A] = writerT((w, a))

  def putF[F[_]: Functor, W, A](fa: F[A])(w: W): WriterT[F, W, A] = WriterT(Functor[F].map(fa)((w, _)))

  def tell[F[_]: Applicative, W](w: W): WriterT[F, W, Unit] = writerT((w, ()))

  def value[F[_]: Applicative, W: Monoid, A](a: A): WriterT[F, W, A] = pure(a)

  def valueF[F[_]: Functor, W: Monoid, A](fa: F[A]): WriterT[F, W, A] = liftF(fa)
}

trait WriterTImplicits {
  implicit def writerTisFunctor[F[_]: Functor, W]: Functor[({type f[x] = WriterT[F, W, x]})#f] =
    new Functor[({type f[x] = WriterT[F, W, x]})#f] {
      def map[A, B](w: WriterT[F, W, A])(f: A => B): WriterT[F, W, B] = w map f
    }

  implicit def writerTisApplicative[F[_]: Applicative: Functor, W: Monoid]: Applicative[({type f[x] = WriterT[F, W, x]})#f] =
    new Applicative[({type f[x] = WriterT[F, W, x]})#f] {
      val functor: Functor[({type f[x] = WriterT[F, W, x]})#f] = Functor[({type f[x] = WriterT[F, W, x]})#f]
      def pure[A](a: => A): WriterT[F, W, A] = WriterT(Applicative[F].pure((Monoid[W].unit, a)))
      def apply[A, B](wf: WriterT[F, W, A => B])(wa: WriterT[F, W, A]): WriterT[F, W, B] = wa apply wf
    }

  implicit def writerTisMonad[F[_]: Monad: Applicative: Functor, W: Monoid]: Monad[({type f[x] = WriterT[F, W, x]})#f] =
    new Monad[({type f[x] = WriterT[F, W, x]})#f] {
      val applicative: Applicative[({type f[x] = WriterT[F, W, x]})#f] = Applicative[({type f[x] = WriterT[F, W, x]})#f]
      def flatMap[A, B](wa: WriterT[F, W, A])(f: A => WriterT[F, W, B]): WriterT[F, W, B] = wa flatMap f
    }

  implicit def writerTisFoldable[F[_]: Foldable, W]: Foldable[({type f[x] = WriterT[F, W, x]})#f] =
    new Foldable[({type f[x] = WriterT[F, W, x]})#f] {
      def foldLeft[A, B](wa: WriterT[F, W, A], z: B)(f: (B, A) => B): B = wa.foldLeft(z)(f)
      def foldRight[A, B](wa: WriterT[F, W, A], z: B)(f: (A, B) => B): B = wa.foldRight(z)(f)
      def foldMap[A, B](wa: WriterT[F, W, A])(f: A => B)(implicit M: Monoid[B]): B = wa.foldMap(f)
    }

  implicit def writerTisTraversable[F[_]: Traverse: Functor: Foldable, W]: Traverse[({type f[x] = WriterT[F, W, x]})#f] =
    new Traverse[({type f[x] = WriterT[F, W, x]})#f] {
      val foldable: Foldable[({type f[x] = WriterT[F, W, x]})#f] = Foldable[({type f[x] = WriterT[F, W, x]})#f]
      val functor: Functor[({type f[x] = WriterT[F, W, x]})#f] = Functor[({type f[x] = WriterT[F, W, x]})#f]
      def traverse[G[_], A, B](wa: WriterT[F, W, A])(f: A => G[B])(implicit G: Applicative[G]): G[WriterT[F,W,B]] =
        wa.traverse(f)
    }

  implicit def writerTisMonoid[F[_]: Applicative, W, A](implicit M: Monoid[F[(W, A)]]): Monoid[WriterT[F, W, A]] =
    new Monoid[WriterT[F, W, A]] {
      val unit: WriterT[F, W, A] = WriterT(M.unit)
      def combine(l: WriterT[F, W, A], r: WriterT[F, W, A]): WriterT[F, W, A] = WriterT(M.combine(l.run, r.run))
    }
}
