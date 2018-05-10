package glue
package typeclass

import std.option._

trait Foldable[F[_]] { self =>
  def foldLeft[A, B](as: F[A], z: B)(f: (B, A) => B): B
  def foldRight[A, B](as: F[A], z: B)(f: (A, B) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(implicit M: Monoid[B]): B

  def combine[A](as: F[A])(implicit M: Monoid[A]): A = foldLeft(as, M.unit)(M.combine)
  def toList[A](fa: F[A]): List[A] = foldLeft(fa, List[A]())((t, h) => h :: t).reverse

  def compose[G[_]](implicit G: Foldable[G]): Foldable[({type f[x] = F[G[x]]})#f] =
    new Foldable[({type f[x] = F[G[x]]})#f] {
      def foldLeft[A, B](fga: F[G[A]], z: B)(f: (B, A) => B): B =
        self.foldLeft(fga, z) { (b, ga) => G.foldLeft(ga, b)(f) }
      def foldRight[A, B](fga: F[G[A]], z: B)(f: (A, B) => B): B =
        self.foldRight(fga, z) { (ga, b) => G.foldRight(ga, b)(f) }
      def foldMap[A, B](fga: F[G[A]])(f: A => B)(implicit M: Monoid[B]): B =
        self.foldMap(fga) { ga => G.foldMap(ga)(f) }
    }

  def foldRightM[G[_], A, B](fa: F[A], z: B)(f: (A, B) => G[B])(implicit G: Monad[G]): G[B] =
    foldLeft[A, B => G[B]](fa, G.unit(_))((g, a) => b => G.flatMap(f(a, b))(g))(z)  // foldRight(fa, G.unit(z))((a, fb) => G.flatMap(fb)(b => f(a, b)))

  def foldLeftM[G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] =
    foldRight[A, B => G[B]](fa, G.unit(_))((a, g) => b => G.flatMap(f(b, a))(g))(z) // foldLeft(fa, G.unit(z))((fb, a) => G.flatMap(fb)(b => f(b, a)))

  def foldMapM[G[_], A, B](fa: F[A])(f: A => G[B])(implicit M: Monoid[B], G: Monad[G]): G[B] =
    foldLeftM(fa, M.unit)((b1, a) => G.map(f(a))(b2 => M.combine(b1, b2)))

  def foldMapOpt[A, B: Monoid](fa: F[A])(f: A => B): Option[B] = foldMap(fa)(a => some(f(a)))

  def fold[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)

  def foldOpt[A: Monoid](fa: F[A]): Option[A] = foldMapOpt(fa)(identity)

  def traverseA_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] =
    foldRight(fa, G.unit(()))((a, gu) => G.seqRight(f(a), gu))

  def forA_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] = traverseA_(fa)(f)

  def mapM_[G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Monad[G]): G[Unit] =
    foldRight(fa, G.unit(())) { (a, gu) => G.seq(f(a), gu) }

  def sequenceA_[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[Unit] =
    traverseA_(fga)(identity)

  def sequenceM_[G[_], A](fga: F[G[A]])(implicit G: Monad[G]): G[Unit] =
    traverseA_(fga)(identity)(G.applicative)

  def findLeft[A](fa: F[A])(p: A => Boolean): Option[A] =
    foldLeft(fa, none[A])((b, a) => b.orElse(if (p(a)) some(a) else none[A]))

  def findRight[A](fa: F[A])(p: A => Boolean): Option[A] =
    foldRight(fa, none[A])((a, b) => b.orElse(if (p(a)) some(a) else none[A]))

  def length[A](fa: F[A]): Int = foldLeft(fa, 0)((b, _) => b + 1)
  def count[A](fa: F[A]): Int = length(fa)

  def at[A](fa: F[A], i: Int): Option[A] =
    foldLeft(fa, (none[A], 0)) { case ((b, x), a) =>
      (b.orElse { if (x == i) some(a) else none[A] }, x + 1)
    }._1

  def atOr[A](fa: F[A], a: => A, i: Int): A =
    at(fa, i) getOrElse a

  def any[A](fa: F[A])(p: A => Boolean): Boolean =
    foldRight(fa, false)(p(_) || _)

  def anyM[G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] =
    foldRight(fa, G.unit(false))((a, b) => G.flatMap(p(a))(q => if (q) G.unit(true) else b))

  def all[A](fa: F[A])(p: A => Boolean): Boolean =
    foldRight(fa, true)(p(_) && _)

  def allM[G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] =
    foldRight(fa, G.unit(true))((a, b) => G.flatMap(p(a))(q => if (q) b else G.unit(false)))
}

object Foldable extends FoldableFunctions {
  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F

  object syntax extends FoldableSyntax
}

trait FoldableFunctions {
  def foldLeft[F[_]: Foldable, A, B](fa: F[A], z: B)(f: (B, A) => B): B = Foldable[F].foldLeft(fa, z)(f)
  def foldRight[F[_]: Foldable, A, B](fa: F[A], z: B)(f: (A, B) => B): B = Foldable[F].foldRight(fa, z)(f)
  def foldMap[F[_]: Foldable, A, B: Monoid](fa: F[A])(f: A => B): B = Foldable[F].foldMap(fa)(f)
  def combine[F[_]: Foldable, A: Monoid](fa: F[A]): A = Foldable[F].combine(fa)
  def toList[F[_]: Foldable, A](fa: F[A]): List[A] = Foldable[F].toList(fa)
  def foldRightM[F[_]: Foldable, G[_]: Monad, A, B](fa: F[A], z: B)(f: (A, B) => G[B]): G[B] = Foldable[F].foldRightM(fa, z)(f)
  def foldLeftM[F[_]: Foldable, G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = Foldable[F].foldLeftM(fa, z)(f)
  def foldMapM[F[_]: Foldable, G[_], A, B](fa: F[A])(f: A => G[B])(implicit M: Monoid[B], G: Monad[G]): G[B] = Foldable[F].foldMapM(fa)(f)
  def foldMapOpt[F[_]: Foldable, A, B: Monoid](fa: F[A])(f: A => B): Option[B] = Foldable[F].foldMapOpt(fa)(f)
  def fold[F[_]: Foldable, A: Monoid](fa: F[A]): A = Foldable[F].fold(fa)
  def foldOpt[F[_]: Foldable, A: Monoid](fa: F[A]): Option[A] = Foldable[F].foldOpt(fa)
  def traverseA_[F[_]: Foldable, G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] = Foldable[F].traverseA_(fa)(f)
  def forA_[F[_]: Foldable, G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Applicative[G]): G[Unit] = Foldable[F].forA_(fa)(f)
  def mapM_[F[_]: Foldable, G[_], A, B](fa: F[A])(f: A => G[B])(implicit G: Monad[G]): G[Unit] = Foldable[F].mapM_(fa)(f)
  def sequenceA_[F[_]: Foldable, G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[Unit] = Foldable[F].sequenceA_(fga)
  def sequenceM_[F[_]: Foldable, G[_], A](fga: F[G[A]])(implicit G: Monad[G]): G[Unit] = Foldable[F].sequenceM_(fga)
  def findLeft[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Option[A] = Foldable[F].findLeft(fa)(p)
  def findRight[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Option[A] = Foldable[F].findRight(fa)(p)
  def length[F[_]: Foldable, A](fa: F[A]): Int = Foldable[F].length(fa)
  def count[F[_]: Foldable, A](fa: F[A]): Int = Foldable[F].count(fa)
  def at[F[_]: Foldable, A](fa: F[A], i: Int): Option[A] = Foldable[F].at(fa, i)
  def atOr[F[_]: Foldable, A](fa: F[A], a: => A, i: Int): A = Foldable[F].atOr(fa, a, i)
  def any[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean = Foldable[F].any(fa)(p)
  def anyM[F[_]: Foldable, G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] = Foldable[F].anyM(fa)(p)
  def all[F[_]: Foldable, A](fa: F[A])(p: A => Boolean): Boolean = Foldable[F].all(fa)(p)
  def allM[F[_]: Foldable, G[_], A](fa: F[A])(p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] = Foldable[F].allM(fa)(p)
}

trait FoldableSyntax {
  implicit class FoldableOps[F[_]: Foldable, A](self: F[A]) {
    def foldLeft[B](z: B)(f: (B, A) => B): B = Foldable[F].foldLeft(self, z)(f)
    def foldRight[B](z: B)(f: (A, B) => B): B = Foldable[F].foldRight(self, z)(f)
    def foldMap[B](f: A => B)(implicit M: Monoid[B]): B = Foldable[F].foldMap(self)(f)

    def combine(implicit M: Monoid[A]): A = Foldable[F].combine(self)
    def toList: List[A] = Foldable[F].toList(self)

    def foldRightM[G[_]: Monad, B](z: B)(f: (A, B) => G[B]): G[B] = Foldable[F].foldRightM(self, z)(f)
    def foldLeftM[G[_], B](z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = Foldable[F].foldLeftM(self, z)(f)
    def foldMapM[G[_], B](f: A => G[B])(implicit M: Monoid[B], G: Monad[G]): G[B] = Foldable[F].foldMapM(self)(f)
    def foldMapOpt[B: Monoid](f: A => B): Option[B] = Foldable[F].foldMapOpt(self)(f)
    def fold(implicit M: Monoid[A]): A = Foldable[F].fold(self)
    def foldOpt(implicit M: Monoid[A]): Option[A] = Foldable[F].foldOpt(self)
    def traverseA_[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[Unit] = Foldable[F].traverseA_(self)(f)
    def forA_[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[Unit] = Foldable[F].forA_(self)(f)
    def mapM_[G[_], B](f: A => G[B])(implicit G: Monad[G]): G[Unit] = Foldable[F].mapM_(self)(f)
    def sequenceA_[G[_]](fga: F[G[A]])(implicit G: Applicative[G]): G[Unit] = Foldable[F].sequenceA_(fga)
    def sequenceM_[G[_]](fga: F[G[A]])(implicit G: Monad[G]): G[Unit] = Foldable[F].sequenceM_(fga)
    def findLeft(p: A => Boolean): Option[A] = Foldable[F].findLeft(self)(p)
    def findRight(p: A => Boolean): Option[A] = Foldable[F].findRight(self)(p)
    def length: Int = Foldable[F].length(self)
    def count: Int = Foldable[F].count(self)
    def at(i: Int): Option[A] = Foldable[F].at(self, i)
    def atOr(a: => A, i: Int): A = Foldable[F].atOr(self, a, i)
    def any(p: A => Boolean): Boolean = Foldable[F].any(self)(p)
    def anyM[G[_]](p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] = Foldable[F].anyM(self)(p)
    def all(p: A => Boolean): Boolean = Foldable[F].all(self)(p)
    def allM[G[_]](p: A => G[Boolean])(implicit G: Monad[G]): G[Boolean] = Foldable[F].allM(self)(p)
  }
}
