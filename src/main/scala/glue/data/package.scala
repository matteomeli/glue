package glue

package object data {
  type ReaderT[F[_], A, B] = Kleisli[F, A, B]
  val ReaderT = Kleisli

  type Reader[A, B] = ReaderT[Id, A, B]
  object Reader extends ReaderFunctions {
    object implicits extends ReaderImplicits
  }
}
