package glue.typeclass

trait Monoid[A] {
  val unit: A
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
    val unit: String = ""
    def combine(a1: String, a2: String): String = a1 + a2
  }

  implicit def listIsMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    val unit: List[A] = List.empty
    def combine(l: List[A], r: List[A]): List[A] = l ++ r
  }
}
