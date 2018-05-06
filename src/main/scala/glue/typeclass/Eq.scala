package glue.typeclass

trait Eq[A] {
  def eqv(x: A, y: A): Boolean
  def neqv(x: A, y: A): Boolean = !eqv(x, y)
}

object Eq extends EqFunctions {
  def apply[A](implicit E: Eq[A]): Eq[A] = E

  def fromEqualityA[A]: Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = x == y
  }

  def fromRefEqualityE[A <: AnyRef]: Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = x eq y
  }

  def fromFunction[A](f: (A, A) => Boolean): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = f(x, y)
  }

  def alwaysEqual[A]: Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = true
  }

  def and[A](e1: Eq[A], e2: Eq[A]): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = e1.eqv(x, y) && e2.eqv(x, y)
  }

  def or[A](e1: Eq[A], e2: Eq[A]): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = e1.eqv(x, y) || e2.eqv(x, y)
  }

  def not[A](e: Eq[A]): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = !e.eqv(x, y)
  }

  def by[A, B](f: A => B)(implicit E: Eq[B]): Eq[A] = new Eq[A] {
    def eqv(x: A, y: A): Boolean = E.eqv(f(x), f(y))
  }

  object syntax extends EqSyntax
}

trait EqFunctions {
  def eqv[A: Eq](x: A, y: A): Boolean = Eq[A].eqv(x, y)
  def neqv[A: Eq](x: A, y: A): Boolean = Eq[A].neqv(x, y)

  implicit def eqIsEquiv[A: Eq]: Equiv[A] = new Equiv[A] {
    def equiv(x: A, y: A) = Eq[A].eqv(x, y)
  }
}

trait EqSyntax {
  implicit class EqOps[A: Eq](self: A) {
    def eqv(other: A): Boolean = Eq[A].eqv(self, other)
    def neqv(other: A): Boolean = Eq[A].neqv(self, other)
  }
}

trait EqLaws[A] {
  implicit val eq: Eq[A]

  import Eq._

  def simmetry(x: A, y: A): Boolean = eqv(x, y) == eqv(y, x)
  def reflexivity(x: A): Boolean = eqv(x, x)
  def transitivity(x: A, y: A, z: A): Boolean = !(eqv(x, y) && eqv(y, z)) || eqv(x, z)
}

object EqLaws {
  def apply[A](implicit E: Eq[A]): EqLaws[A] =
    new EqLaws[A] { val eq: Eq[A] = E }
}
