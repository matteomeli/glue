package glue
package effect

sealed trait Free[F[_], A] {
  import Free._

  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Pure(_)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Chain(this, f)
  def applyF[B](f: Free[F, A => B]): Free[F, B] = flatMap { a => f.map(_(a)) }

  def step: Free[F, A] = Free.step(this)

  def run(implicit ev: Free[F, A] =:= Trampoline[A]): A = Free.run(ev(this))
  def runM(implicit F: Monad[F]): F[A] = Free.runM(this)
}

case class Pure[F[_], A](a: A) extends Free[F, A]
case class Effect[F[_], A](e: F[A]) extends Free[F, A]
case class Chain[F[_], A0, A](v: Free[F, A0], f: A0 => Free[F, A]) extends Free[F, A]

object Free extends FreeFunctions {
  type Trampoline[A] = Free[Function0, A]

  // The IO type: for now only provides trampolined sequential execution (stack safe)
  // TODO: Add asynchronous execution, using an F that provides primitives for parallelism, 
  // i.e. wraps Future and providesasynchronous reads and writes.
  // Then for every set of I/O operations - file access, database access, network, stdin/stdout in console -
  // we just have to provide an ADT representing the operations, say F. For type F, we could generate
  // a free monad Free[F, A] in which to write our `programs`. These programs are just descriptions,
  // they can be tested individually and then compiled down to our final lower level type IO 
  // supporting trampolined and asynchronous execution.
  type IO[A] = Free.Trampoline[A]

  @annotation.tailrec
  def run[A](t: Trampoline[A]): A = t match {
    case Pure(a) => a
    case Effect(e) => e()
    case Chain(x, f) => x match {
      case Pure(a) => run(f(a))
      case Effect(e) => run(f(e()))
      case Chain(y, g) => run(y flatMap (g andThen f))
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

object Trampoline {
  import Free._

  def done[A](a: A): Trampoline[A] = Free.pure[Function0, A](a)

  def suspend[A](a: => Trampoline[A]) = Free.suspend(a)

  def delay[A](a: => A): Trampoline[A] = suspend(done(a))

  object implicits extends TrampolineImplicits
}

trait FreeFunctions {
  def pure[F[_], A](a: A): Free[F, A] = Pure[F, A](a)

  def unit[F[_]]: Free[F, Unit] = pure[Id.Id, Unit](()).asInstanceOf[Free[F, Unit]]

  def suspend[F[_], A](fa: => Free[F, A]): Free[F, A] = unit flatMap { _ => fa }

  def map[F[_], A, B](v: Free[F, A])(f: A => B): Free[F, B] = v map f
  def flatMap[F[_], A, B](v: Free[F, A])(f: A => Free[F, B]): Free[F, B] = v flatMap f
  def applyF[F[_], A, B](ff: Free[F, A => B])(fa: Free[F, A]): Free[F, B] = fa applyF ff

  def liftF[F[_], A](fa: F[A]): Free[F, A] = Effect(fa)
}

trait FreeImplicits {
  private implicit def freeFunctor[F[_]]: Functor[({type f[x] = Free[F, x]})#f] =
    new Functor[({type f[x] = Free[F, x]})#f] {
      def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa map f
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
      def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
    }
}

trait TrampolineImplicits {
  import Free._

  private implicit val trampolineFunctor: Functor[Trampoline] =
    new Functor[Trampoline] {
      def map[A, B](ta: Trampoline[A])(f: A => B): Trampoline[B] = ta map f
    }

  private implicit val trampolineApplicative: Applicative[Trampoline] =
    new Applicative[Trampoline] {
      val functor: Functor[Trampoline] = Functor[Trampoline]
      def pure[A](a: => A): Trampoline[A] = Trampoline.done(a)
      def apply[A, B](tf: Trampoline[A => B])(fa: Trampoline[A]): Trampoline[B] = fa applyF tf
    }

  implicit val trampolineMonad: Monad[Trampoline] =
    new Monad[Trampoline] {
      val applicative: Applicative[Trampoline] = Applicative[Trampoline]
      def flatMap[A, B](ta: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] = ta flatMap f
    }
}

object SafeIOTest {
  import Free._
  import Free.implicits._
  def printLine(s: String): IO[Unit] = Effect(() => Pure(println(s)))
  val foreverMore = Monad[IO].forever(printLine("More."))
  //foreverMore.run
}