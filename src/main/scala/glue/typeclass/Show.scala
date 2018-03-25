package glue.typeclass

trait Show[A] {
  def show(a: A): String
}

object Show extends ShowFunctions {
  def apply[A](implicit S: Show[A]): Show[A] = S

  object syntax extends ShowSyntax

  object instances extends ShowInstances
}

trait ShowFunctions {
  def show[A: Show](a: A): String = Show[A].show(a)
}

trait ShowSyntax {
  implicit class ShowOps[A: Show](self: A) {
    def show = Show[A].show(self)
  }
}

trait ShowInstances {
  implicit val intCanShow: Show[Int] = int => s"int $int"
  implicit val stringCanShow: Show[String] = str => s"string $str"
}
