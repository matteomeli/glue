package glue.typeclass

import scala.language.implicitConversions

trait Show[A] {
  def show(a: A): String
}

object Show extends ShowInstances {
  def apply[A](implicit S: Show[A]): Show[A] = S

  object syntax extends ShowSyntax
}

trait ShowSyntax {
  def show[A: Show](a: A): String = Show[A].show(a)

  implicit class ShowOps[A: Show](self: A) {
    def show = Show[A].show(self)
  }
}

trait ShowInstances {
  implicit val intCanShow: Show[Int] = int => s"int $int"
  implicit val stringCanShow: Show[String] = str => s"string $str"
}
