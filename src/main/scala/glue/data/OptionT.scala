package glue.data

import glue.std.option._
import glue.typeclass.{Applicative, Functor, Monad}
import glue.NaturalTransformation

case class OptionT[F[_], A](run: F[Option[A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): OptionT[F, B] = OptionT(F.map(run)(_.map(f)))

  def mapT[G[_], B](f: F[Option[A]] => G[Option[B]]): OptionT[G, B] = OptionT(f(run))

  def mapN[G[_]](t: NaturalTransformation[F, G]): OptionT[G, A] = OptionT(t(run))

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
  implicit def optionTIsMonad[F[_]: Monad]: Monad[({type f[x] = OptionT[F, x]})#f] =
    new Monad[({type f[x] = OptionT[F, x]})#f] {
      val applicative: Applicative[({type f[x] = OptionT[F, x]})#f] = new Applicative[({type f[x] = OptionT[F, x]})#f] {
        val functor: Functor[({type f[x] = OptionT[F, x]})#f] = optionTIsFunctor(Monad[F].applicative.functor)
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
