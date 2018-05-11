package glue
package data

case class IdT[F[_], A](run: F[Id[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): IdT[F, B] = IdT(F.map(run)(f))

  def mapT[G[_], B](f: F[Id[A]] => G[Id[B]]): IdT[G, B] = IdT(f(run))

  def mapK[G[_]](k: NaturalTransformation[F, G]): IdT[G, A] = IdT(k(run))

  def mapF[B](f: A => F[B])(implicit F: Monad[F]): IdT[F, B] = IdT(F.flatMap(run)(f))

  def flatMap[B](f: A => IdT[F, B])(implicit F: Monad[F]): IdT[F, B] = IdT(F.flatMap(run)(f(_).run))

  def flatMapF[B](f: A => F[Id[B]])(implicit F: Monad[F]): IdT[F, B] = IdT(F.flatMap(run)(f))

  def apply[B](ff: IdT[F, A => B])(implicit F: Applicative[F]): IdT[F, B] = IdT {
    F.map2(ff.run, run)(_(_))
  }

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit F: Foldable[F]): B = F.foldLeft(run, z)(f)

  def foldRight[B](z: B)(f: (A, B) => B)(implicit F: Foldable[F]): B = F.foldRight(run, z)(f)

  def foldMap[B](f: A => B)(implicit F: Foldable[F], M: Monoid[B]): B = F.foldMap(run)(f)

  def traverse[G[_]: Applicative, B](f: A => G[B])(implicit T: Traverse[F]): G[IdT[F, B]] =
    Applicative[G].map(T.traverse(run)(f))(IdT(_))
}

object IdT extends IdTFunctions {
  object implicits extends IdTImplicits
}

trait IdTFunctions {
  def idT[F[_]]: NaturalTransformation[({type f[x] = F[Id[x]]})#f, ({type f[x] = IdT[F, x]})#f] =
    new NaturalTransformation[({type f[x] = F[Id[x]]})#f, ({type f[x] = IdT[F, x]})#f] {
      def apply[A](fa: F[A]): IdT[F, A] = IdT(fa)
    }
}

trait IdTImplicits {
  private implicit def idTIsFunctor[F[_]: Functor]: Functor[({type f[x] = IdT[F, x]})#f] =
    new Functor[({type f[x] = IdT[F, x]})#f] {
      def map[A, B](fa: IdT[F,A])(f: A => B): IdT[F,B] = fa map f
    }

  private implicit def idTIsApplicative[F[_]: Applicative: Functor]: Applicative[({type f[x] = IdT[F, x]})#f] =
    new Applicative[({type f[x] = IdT[F, x]})#f] {
      val functor: Functor[({type f[x] = IdT[F, x]})#f] = Functor[({type f[x] = IdT[F, x]})#f]
      def unit[A](a: => A): IdT[F, A] = IdT(Applicative[F].unit(a))
      def apply[A, B](ff: IdT[F, A => B])(fa: IdT[F,A]): IdT[F, B] = fa apply ff
    }

  implicit def idTIsMonad[F[_]: Monad: Applicative: Functor]: Monad[({type f[x] = IdT[F, x]})#f] =
    new Monad[({type f[x] = IdT[F, x]})#f] {
      val applicative: Applicative[({type f[x] = IdT[F, x]})#f] = Applicative[({type f[x] = IdT[F, x]})#f]
      def flatMap[A, B](ma: IdT[F, A])(f: A => IdT[F, B]): IdT[F, B] = ma flatMap f
    }
}
