package glue.typeclass

trait Monoid[A] {
  def unit: A
  def combine(a1: A, a2: A): A
  def combineAll(as: TraversableOnce[A]): A = as.foldLeft(unit)(combine)
}

object Monoid {
  def apply[A](implicit M: Monoid[A]) = M

  object syntax extends MonoidSyntax

  object intances extends MonoidInstances
}

trait MonoidSyntax {
  def unit[A: Monoid]: A = Monoid[A].unit
  def combine[A: Monoid](a1: A, a2: A): A = Monoid[A].combine(a1, a2)
  def combineAll[A: Monoid](as: TraversableOnce[A]): A = Monoid[A].combineAll(as)

  implicit class MonoidOps[A: Monoid](al: A) {
    def combine(ar: A): A = Monoid[A].combine(al, ar)
  }
}

trait MonoidInstances {
  implicit val stringIsMonoid: Monoid[String] = new Monoid[String] {
    def unit: String = ""
    def combine(a1: String, a2: String): String = a1 + a2
  }
}
