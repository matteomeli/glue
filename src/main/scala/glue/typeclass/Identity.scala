package glue.typeclass

case class Identity[A](run: A) extends AnyVal

object Identity {
  type Id[A] = A
}