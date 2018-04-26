package glue.typeclass

trait Monoid[A] {
  val unit: A
  def combine(a1: A, a2: A): A
  def combineAll(as: TraversableOnce[A]): A = as.foldLeft(unit)(combine)
}

object Monoid extends MonoidFunctions {
  def apply[A](implicit M: Monoid[A]): Monoid[A] = M

  object syntax extends MonoidSyntax

  object instances extends MonoidInstances
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

trait MonoidInstances {
  implicit def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    val unit: A => A = identity[A]
    def combine(a1: A => A, a2: A => A): A => A = a1 compose a2
  }
}

trait MonoidLaws[A] {
  implicit def M: Monoid[A]

  def leftIdentity(a: A): Boolean = M.combine(M.unit, a) == a
  def rightIdentity(a: A): Boolean = M.combine(a, M.unit) == a
  def associativity(a1: A, a2: A, a3: A): Boolean =
    M.combine(a1, M.combine(a2, a3)) == M.combine(M.combine(a1, a2), a3)
}

object MonoidLaws {
  def apply[A](implicit ev: Monoid[A]): MonoidLaws[A] =
    new MonoidLaws[A] { def M: Monoid[A] = ev }
}
