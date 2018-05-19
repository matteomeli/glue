# Glue

Glue is an implementation study of useful category theory concepts for functional programming in Scala.

It is developed using zero external dependencies.

Its main differentiating characteristic compared to other similar libraries is the _encoding_ of typeclasses.

The common encoding of typeclasses (in Scala) relies on subtyping, like in [Cats](https://github.com/typelevel/cats) and [Scalaz](https://github.com/scalaz/scalaz). This allows the user of those libraries to write clean code but it comes with a cost. As an example for Cats:

```scala
// This is the usual encoding through inheritance
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}
trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def apply[A, B](fa: F[A])(fab: F[A => B]): F[B]
}
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
trait Traverse[F[_]] extends Functor[F] {
  def traverse[G[_], A, B](fa: F[A])(f: A => G[A])(implicit G: Applicative[G]): G[F[A]]
}
```

Additional syntax provided by the library allows us to call functions like `map`, `flatMap` and `traverse` directly on some `F[A]`, assuming that there is an implciit evidence in scope that `F` is an instance of the appropriate typeclass (`Functor `, `Monad`, `Applicative` and `Traverse` in this case).

```scala
import cats._
import cats.implicits._

def foo[F[_]: Monad]: F[Int] = for {
  a <- Monad[F].pure(10)
  b <- Monad[F].pure(20)
} yield a + b
```

Bear in mind that because for comprehension in Scala desugar, there are calls to `map` and `flatMap` hidden in there. And the fact that, thorugh subtyping the compiler knows that a `Monad[F]` implies `Functor[F]`, so all works out. But does it always? What about this:

```scala
def foo[F[_]: Monad: Traverse]: F[Int] = Monad[F].pure(10).map(identity)
```

Ignore the fact we're not even using `Traverse` - we can't even call `map`! The compiler bails out like this:

```scala
// <console>:19: error: value map is not a member of type parameter F[Int]
//        def foo[F[_]: Monad: Traverse]: F[Int] = Monad[F].pure(10).map(identity)
//                                                                   ^
// <console>:19: error: missing argument list for method identity in object Predef
// Unapplied methods are only converted to functions when a function type is expected.
// You can make this conversion explicit by writing `identity _` or `identity(_)` instead of `identity`.
//        def foo[F[_]: Monad: Traverse]: F[Int] = Monad[F].pure(10).map(identity)
//
```

To have access to `map` we need `F` to be an instance of `Functor`, which it does via `Monad` as before, but now also via `Traverse`. And this is why ultimately it does not work in this and all similar cases, because the encoding of typeclasses uses subtyping, a `Monad[F]` is a `Functor[F]`. But a `Traverse[F]` is a `Functor[F]`! When the compiler attempts to resolve the implicits in scope to find a Functor[F], it can’t decide between `Monad[F]`’s or `Traverse[F]`’s and fails. Even though the instances are (or should be) the same the compiler has no way of knowing that. And this will happen every time the compiler will encounter ambiguos implicits caused by two typeclasses sharing a common ancestor.

This library proposes a different approach. Throw away the encoding via subtyping and use trait members instead:

```scala
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

trait Applicative[F[_]] {
  val functor: Functor[F]

  def pure[A](a: => A): F[A]
  def apply[A, B](f: F[A => B])(fa: F[A]): F[B]
}

trait Monad[F[_]] {
  val applicative: Applicative[F]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
```

Future work includes:

* Bifunctor
* Bifoldable
* Bitraverse
* Cokleisli
* Comonad
* MonadError, MonadPlus, MonadRec
* Alternative
* Yoneda, Coyoneda
* Free
* Par
* ST, STRef, STArray
* IO
* Streaming IO
* Arrows

Future related work includes:
* Parallel
* Lenses
* Zipper
* Parser
* FRP
