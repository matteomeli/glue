package glue.typeclass

trait Monoid[A] { self =>
  val unit: A
  def combine(a1: A, a2: A): A
  def combineAll(as: TraversableOnce[A]): A = as.foldLeft(unit)(combine)

  // The product of two monoids on types A and B is a monoid of type (A, B)
  def product[B](implicit M: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    val unit: (A, B) = (self.unit, M.unit)
    def combine(x: (A, B), y: (A, B)): (A, B) = (self.combine(x._1, y._1), M.combine(x._2, y._2))
  }
}

object Monoid extends MonoidFunctions {
  def apply[A](implicit M: Monoid[A]): Monoid[A] = M

  def mapMerge[K, V](implicit V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    val unit: Map[K, V] = Map[K, V]()
    def combine(m1: Map[K, V], m2: Map[K, V]): Map[K, V] =
      (m1.keySet ++ m2.keySet).foldLeft(unit) { (acc, k) =>
        acc.updated(k, V.combine(m1.getOrElse(k, V.unit), m2.getOrElse(k, V.unit)))
      }
  }

  def functionMerge[A, B](implicit M: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    val unit: A => B = _ => M.unit
    def combine(f: A => B, g: A => B): A => B = a => M.combine(f(a), g(a))
  }

  object syntax extends MonoidSyntax

  object implicits extends MonoidImplicits
}

trait MonoidFunctions {
  def unit[A: Monoid]: A = Monoid[A].unit
  def combine[A: Monoid](a1: A, a2: A): A = Monoid[A].combine(a1, a2)
  def combineAll[A: Monoid](as: TraversableOnce[A]): A = Monoid[A].combineAll(as)
}

trait MonoidSyntax {
  implicit class MonoidOps[A: Monoid](self: A) {
    def combine(other: A): A = Monoid[A].combine(self, other)
  }
}

trait MonoidImplicits {
  import glue.data.Const
  import glue.data.Const.implicits._

  implicit def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    val unit: A => A = identity[A]
    def combine(a1: A => A, a2: A => A): A => A = a1 compose a2
  }

  implicit def monoidIsApplicative[M: Monoid]: Applicative[({type f[x] = Const[M, x]})#f] =
    new Applicative[({type f[x] = Const[M, x]})#f] {
      val functor: Functor[({type f[x] = Const[M, x]})#f] = Functor[({type f[x] = Const[M, x]})#f]
      def unit[A](a: => A): Const[M, A] = Const(Monoid[M].unit)
      def apply[A, B](f: Const[M, A => B])(fa: Const[M, A]): Const[M, B] =
        Const(Monoid[M].combine(f.run, fa.run))
    }
}

trait MonoidLaws[A] {
  implicit def monoid: Monoid[A]

  def leftIdentity(a: A): Boolean = monoid.combine(monoid.unit, a) == a
  def rightIdentity(a: A): Boolean = monoid.combine(a, monoid.unit) == a
  def associativity(a1: A, a2: A, a3: A): Boolean =
    monoid.combine(a1, monoid.combine(a2, a3)) == monoid.combine(monoid.combine(a1, a2), a3)
}

object MonoidLaws {
  def apply[A](implicit M: Monoid[A]): MonoidLaws[A] =
    new MonoidLaws[A] { def monoid: Monoid[A] = M }
}
