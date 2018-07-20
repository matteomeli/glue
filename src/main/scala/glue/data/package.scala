package glue
package object data {

  type ReaderT[F[_], R, A] = Kleisli[F, R, A]
  val ReaderT = Kleisli

  type Reader[R, A] = ReaderT[Id, R, A]
  val Reader = Kleisli

  type Writer[R, A] = WriterT[Id, R, A]
  val Writer = WriterT

  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
  val StateT = IndexedStateT

  type State[R, A] = StateT[Id, R, A]
  val State = IndexedStateT

}