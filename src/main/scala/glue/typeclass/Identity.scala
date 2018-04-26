package glue.typeclass

case class Identity[A](run: A) extends AnyVal

object Identity {
  type Id[A] = A
}

trait IdImplicits {
  import Identity._

  implicit val idIsFunctor: Functor[Id] = new Functor[Id] {
    def map[A, B](id: Id[A])(f: A => B): Id[B] = f(id)
  }
}