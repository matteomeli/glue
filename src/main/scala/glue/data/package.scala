package glue

package object data {
  type ReaderT[F[_], A, B] = Kleisli[F, A, B]
  val ReaderT = Kleisli

  type Reader[A, B] = ReaderT[Id, A, B]
  object Reader extends ReaderFunctions {
    object implicits extends ReaderImplicits
  }

  type Writer[W, A] = WriterT[Id, W, A]
  object Writer extends WriterFunctions {
    object implicits extends WriterImplicits
  }
}
