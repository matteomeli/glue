package glue.typeclass

trait Monoid[A] {
  def unit: A
  def combine(a1: A, a2: A): A
  def combineAll(as: TraversableOnce[A]): A = as.foldLeft(unit)(combine)
}

object Monoid extends MonoidInstances {
  def apply[A](implicit M: Monoid[A]): Monoid[A] = M

  object syntax extends MonoidSyntax
}

trait MonoidSyntax {
  def unit[A: Monoid]: A = Monoid[A].unit
  def combine[A: Monoid](a1: A, a2: A): A = Monoid[A].combine(a1, a2)
  def combineAll[A: Monoid](as: TraversableOnce[A]): A = Monoid[A].combineAll(as)

  implicit class MonoidOps[A: Monoid](self: A) {
    def combine(other: A): A = Monoid[A].combine(self, other)
  }
}

trait MonoidInstances {
  implicit val stringIsMonoid: Monoid[String] = new Monoid[String] {
    override def unit: String = ""
    override def combine(a1: String, a2: String): String = a1 + a2
  }
}
