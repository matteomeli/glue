package glue
package effect

sealed trait Free[F[_], A] {
  import Free._

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Pure(_)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Chain(this, f)
  def applyF[B](f: Free[F, A => B]): Free[F, B] = flatMap { a => f.map(_(a)) }

  def run(implicit ev: Free[F, A] =:= Trampoline[A]): A = runTrampoline(ev(this))

  def step: Free[F, A] = Free.step(this)

  def runM(implicit F: Monad[F]): F[A] = Free.runM(this)
}

case class Pure[F[_], A](a: A) extends Free[F, A]
case class Effect[F[_], A](e: F[A]) extends Free[F, A]
case class Chain[F[_], A0, A](v: Free[F, A0], f: A0 => Free[F, A]) extends Free[F, A]

object Free extends FreeFunctions {
  type Trampoline[A] = Free[Function0, A]
  type IO[A] = Free.Trampoline[A]

  @annotation.tailrec
  def runTrampoline[A](t: Trampoline[A]): A = t match {
    case Pure(a) => a
    case Effect(e) => e()
    case Chain(x, f) => x match {
      case Pure(a) => runTrampoline(f(a))
      case Effect(e) => runTrampoline(f(e()))
      case Chain(y, g) => runTrampoline(y flatMap (g andThen f))
    }
  }

  @annotation.tailrec
  final def step[F[_], A](fa: Free[F, A]): Free[F, A] = fa match {
    case Chain(Chain(x, f), g) => step(x flatMap (f andThen g))
    case Chain(Pure(x), f) => step(f(x))
    case _ => fa
  }

  def runM[F[_], A](fa: Free[F, A])(implicit F: Monad[F]): F[A] = step(fa) match {
    case Pure(a) => F.pure(a)
    case Effect(e) => e
    case Chain(Effect(e), f) => Monad[F].flatMap(e) { a => runM(f(a)) }
    case _ => sys.error("Impossible since `step` eliminate these cases.")
  }

  def runFree[F[_], G[_], A](fa: Free[F, A])(k: NaturalTransformation[F, G])(implicit G: Monad[G]): G[A] = step(fa) match {
    case Pure(a) => G.unit(a)
    case Effect(e) => k(e)
    case Chain(Effect(e), f) => G.flatMap(k(e)) { a => runFree(f(a))(k) }
    case _ => sys.error("Impossible since `step` eliminates these cases.")
  }

  def translate[F[_], G[_], A](f: Free[F, A])(k: NaturalTransformation[F, G]): Free[G, A] = {
    import implicits._
    type FreeG[Z] = Free[G, Z]
    val K: NaturalTransformation[F, FreeG] =
      new NaturalTransformation[F, FreeG] {
        def apply[Z](f: F[Z]): Free[G, Z] = Effect(k(f))
      }
    runFree(f)(K)(Monad[FreeG])
  }

  object implicits extends FreeImplicits
}

trait FreeFunctions {
  def map[F[_], A, B](v: Free[F, A])(f: A => B): Free[F, B] = v map f
  def flatMap[F[_], A, B](v: Free[F, A])(f: A => Free[F, B]): Free[F, B] = v flatMap f
  def applyF[F[_], A, B](ff: Free[F, A => B])(fa: Free[F, A]): Free[F, B] = fa applyF ff

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Effect(fa)
}

trait FreeImplicits {
  private implicit def freeFunctor[F[_]]: Functor[({type f[x] = Free[F, x]})#f] =
    new Functor[({type f[x] = Free[F, x]})#f] {
      def map[A, B](v: Free[F, A])(f: A => B): Free[F, B] = v map f
    }

  private implicit def freeApplicative[F[_]]: Applicative[({type f[x] = Free[F, x]})#f] =
    new Applicative[({type f[x] = Free[F, x]})#f] {
      val functor: Functor[({type f[x] = Free[F, x]})#f] = Functor[({type f[x] = Free[F, x]})#f]
      def pure[A](a: => A): Free[F, A] = Pure(a)
      def apply[A, B](ff: Free[F, A => B])(fa: Free[F, A]): Free[F, B] = fa applyF ff
    }

  implicit def freeMonad[F[_]]: Monad[({type f[x] = Free[F, x]})#f] =
    new Monad[({type f[x] = Free[F, x]})#f] {
      val applicative: Applicative[({type f[x] = Free[F, x]})#f] = Applicative[({type f[x] = Free[F, x]})#f]
      def flatMap[A, B](v: Free[F, A])(f: A => Free[F, B]): Free[F, B] = v flatMap f
    }
}

object SafeIOTest {
  import Free._
  import Free.implicits._
  def printLine(s: String): IO[Unit] = Effect(() => Pure(println(s)))
  val foreverMore = Monad[IO].forever(printLine("More."))
  //foreverMore.run
}