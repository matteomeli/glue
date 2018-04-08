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
  implicit val stringIsMonoid: Monoid[String] = new Monoid[String] {
    val unit: String = ""
    def combine(a1: String, a2: String): String = a1 + a2
  }

  implicit def listIsMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    val unit: List[A] = List.empty
    def combine(l: List[A], r: List[A]): List[A] = l ++ r
  }

  implicit def optionIsMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    val unit: Option[A] = None
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
  }

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
  def apply[A](implicit m: Monoid[A]): MonoidLaws[A] =
    new MonoidLaws[A] { def M: Monoid[A] = m }
}
