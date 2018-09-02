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

  type IndexedState[S1, S2, A] = IndexedStateT[Id, S1, S2, A]
  object IndexedState extends StateFunctions {
    def apply[S1, S2, A](f: S1 => (S2, A)): IndexedState[S1, S2, A] = IndexedStateT[Id, S1, S2, A](f)
  }

  type StateT[F[_], S, A] = IndexedStateT[F, S, S, A]
  object StateT extends StateTFunctions {
    def apply[F[_], S, A](f: F[S => F[(S, A)]]): StateT[F, S, A] = IndexedStateT[F, S, S, A](f)

    object implicits extends StateTImplicits
  }

  type State[S, A] = StateT[Id, S, A]
  object State extends StateFunctions {
    def apply[S, A](f: S => (S, A)): State[S, A] = StateT[Id, S, A](f)

    object implicits extends StateImplicits
  }
}
