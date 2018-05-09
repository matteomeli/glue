package glue
package typeclass

trait Show[A] {
  def show(a: A): String
}

object Show extends ShowFunctions {
  def apply[A](implicit S: Show[A]): Show[A] = S

  def show[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  def fromToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }

  final case class InterpolateWithShow(override val toString: String) extends AnyVal
  object InterpolateWithShow {
    implicit def apply[A](a: A)(implicit S: Show[A]): InterpolateWithShow =
      InterpolateWithShow(S.show(a))
  }
  final case class ShowInterpolator(val sc: StringContext) extends AnyVal {
    def show(args: InterpolateWithShow*): String = sc.s(args: _*)
  }

  object syntax extends ShowSyntax
}

trait ShowFunctions {
  def show[A: Show](a: A): String = Show[A].show(a)
}

trait ShowSyntax {
  implicit class ShowOps[A: Show](self: A) {
    def show: String = Show[A].show(self)
    def print(): Unit = Console.print(show)
    def println(): Unit = Console.println(show)
  }

  implicit final def showInterpolator(sc: StringContext): Show.ShowInterpolator =
    Show.ShowInterpolator(sc)
}
