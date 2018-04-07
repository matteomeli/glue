package glue.typeclass

import scala.language.implicitConversions

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

  object syntax extends ShowSyntax

  object instances extends ShowInstances
}

trait ShowFunctions {
  def show[A: Show](a: A): String = Show[A].show(a)
}

object ShowInterpolator {
  final case class ToShow(override val toString: String) extends AnyVal
  object ToShow { implicit def apply[A](a: A)(implicit S: Show[A]): ToShow = ToShow(S.show(a)) }
  final case class ShowStringContext(val sc: StringContext) extends AnyVal {
    def show(args: ToShow*): String = sc.s(args: _*)
  }
}

trait ShowSyntax {
  implicit class ShowOps[A: Show](self: A) {
    def show = Show[A].show(self)
  }

  implicit final def showStringContext(sc: StringContext): ShowInterpolator.ShowStringContext =
    ShowInterpolator.ShowStringContext(sc)
}

trait ShowInstances {
  implicit val intCanShow: Show[Int] = int => s"int $int"
  implicit val stringCanShow: Show[String] = str => s"string $str"
}
