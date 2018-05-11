package glue
package data

import glue.std.option._

case class OptionT[F[_], A](run: F[Option[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(run)(_.map(f)))

  def mapT[G[_], B](f: F[Option[A]] => G[Option[B]]): OptionT[G, B] = OptionT(f(run))

  def mapK[G[_]](k: NaturalTransformation[F, G]): OptionT[G, A] = OptionT(k(run))

  def mapF[B](f: A => F[B])(implicit F: Monad[F]): OptionT[F, B] = OptionT {
    F.flatMap(run) {
      case None => F.unit(none[B])
      case Some(a) => F.map(f(a))(b => some(b))
    }
  }

  def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] = OptionT {
    F.flatMap(run) {
      case None => F.unit(none[B])
      case Some(a) => f(a).run
    }
  }

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] = OptionT {
    F.flatMap(run) {
      case None => F.unit(none[B])
      case Some(a) => f(a)
    }
  }

  def apply[B](of: => OptionT[F, A => B])(implicit F: Monad[F]): OptionT[F, B] = OptionT {
    F.flatMap(of.run) {
      case None => F.unit(none[B])
      case Some(f) => F.map(run)(_ map f)
    }
  }

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit F: Foldable[F]): B =
    F.compose(optionIsTraversable.foldable).foldLeft(run, z)(f)

  def foldRight[B](z: B)(f: (A, B) => B)(implicit F: Foldable[F]): B =
    F.compose(optionIsTraversable.foldable).foldRight(run, z)(f)

  def foldMap[B](f: A => B)(implicit F: Foldable[F], M: Monoid[B]): B =
    F.compose(optionIsTraversable.foldable).foldMap(run)(f)

  def traverse[G[_]: Applicative, B](f: A => G[B])(implicit T: Traverse[F]): G[OptionT[F, B]] =
    Applicative[G].map(T.compose(Traverse[Option]).traverse(run)(f))(OptionT(_))
}

object OptionT extends OptionTFunctions {
  object implicits extends OptionTImplicits
}

trait OptionTFunctions {
  def optionT[F[_]]: NaturalTransformation[({type f[x] = F[Option[x]]})#f, ({type f[x] = OptionT[F, x]})#f] =
    new NaturalTransformation[({type f[x] = F[Option[x]]})#f, ({type f[x] = OptionT[F, x]})#f] {
      def apply[A](fo: F[Option[A]]): OptionT[F, A] = OptionT(fo)
    }

  def someT[F[_]: Applicative, A](a: => A): OptionT[F, A] = OptionT(Applicative[F].unit(some(a)))
  def noneT[F[_]: Applicative, A]: OptionT[F, A] = OptionT(Applicative[F].unit(none[A]))
}

trait OptionTImplicits {
  implicit def optionTIsMonad[F[_]: Monad: Functor]: Monad[({type f[x] = OptionT[F, x]})#f] =
    new Monad[({type f[x] = OptionT[F, x]})#f] {
      val applicative: Applicative[({type f[x] = OptionT[F, x]})#f] = new Applicative[({type f[x] = OptionT[F, x]})#f] {
        val functor: Functor[({type f[x] = OptionT[F, x]})#f] = Functor[({type f[x] = OptionT[F, x]})#f]
        def unit[A](a: => A): OptionT[F, A] = OptionT(Monad[F].unit(some(a)))
        def apply[A, B](of: OptionT[F, A => B])(oa: OptionT[F, A]): OptionT[F, B] = oa.apply(of)
      }
      def flatMap[A, B](o: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = o flatMap f
    }

  private implicit def optionTIsFunctor[F[_]: Functor]: Functor[({type f[x] = OptionT[F, x]})#f] =
    new Functor[({type f[x] = OptionT[F, x]})#f] {
      def map[A, B](o: OptionT[F, A])(f: A => B): OptionT[F, B] = o map f
    }
}
