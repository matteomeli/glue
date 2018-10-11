package glue
package concurrent

import java.util.concurrent.{Executors, ExecutorService, ThreadFactory}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

sealed trait Par[A] {
  def map[B](f: A => B): Par[B] = Par.map(this)(f)
  def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(this)(f)
  def runAsync(cb: A => Unit): Unit = Par.runAsync(this)(cb)
  def run: A = Par.run(this)
}

object Par {
  case class Now[A](value: A) extends Par[A]
  case class Async[A](k: (A => Unit) => Unit) extends Par[A]
  case class Delay[A](t: () => Par[A]) extends Par[A]
  case class Chain[A, B](pa: Par[A], f: A => Par[B]) extends Par[B]

  def apply[A](a: => A)(implicit es: ExecutorService = ExecutionContext.defaultExecutorService): Par[A] = Async { cb =>
    es.execute { new Runnable { def run = cb(a) } }
  }

  def now[A](a: A): Par[A] = Now(a)

  def delay[A](pa: => Par[A]): Par[A] = Delay(() => pa)

  def async[A](k: (A => Unit) => Unit): Par[A] = Async(k)

  def delayNow[A](a: => A): Par[A] = delay(now(a))

  def lazyNow[A](a: => A)(implicit es: ExecutorService = ExecutionContext.defaultExecutorService): Par[A] = fork(now(a))

  def asyncF[A, B](f: A => B)(implicit es: ExecutorService = ExecutionContext.defaultExecutorService): A => Par[B] = a => lazyNow(f(a))

  def fork[A](pa: => Par[A])(implicit es: ExecutorService = ExecutionContext.defaultExecutorService): Par[A] = join(Par(pa))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = flatMap(pa)(f andThen (now(_)))

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = pa match {
    case Now(a) => Delay(() => f(a))
    case Delay(t) => Chain(Delay(t), f)
    case Async(k) => Chain(Async(k), f)
    case Chain(Now(a), g) => Delay(() => Chain(Now(a), g andThen (_ flatMap f)))
    case Chain(Delay(t), g) => Delay(() => Chain(Delay(t), g andThen (_ flatMap f)))
    case Chain(Async(k), g) => Delay(() => Chain(Async(k), g andThen (_ flatMap f)))
    case Chain(Chain(x, h), g) => Delay(() => Chain(x, h andThen (g andThen (_ flatMap f))))
  }

  def join[A](pa: Par[Par[A]]): Par[A] = flatMap(pa)(identity)

  // Not parallel
  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = for {
    a <- pa
    b <- pb
  } yield f(a, b)

  // Parallel map2
  def parMap2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = flatMap(choose(pa, pb)) {
    case Left((a, rb)) => map(rb)(b => f(a, b))
    case Right((ra, b)) => map(ra)(a => f(a, b))
  }

  def both[A, B](pa: Par[A], pb: Par[B]): Par[(A, B)] = parMap2(pa, pb)((_, _))

  // chooseAny and choose can be implemented in terms of each other
  def chooseAny[A](as: List[Par[A]]): Option[Par[(A, List[Par[A]])]] = as match {
    case Nil => None
    case h :: Nil => Some { Async { cb =>
      runAsync(h) { a => cb((a, Nil)) }
    }}
    case h :: t => chooseAny(t).map { pt =>
      flatMap(choose(h, pt)) {
        case Left((a, pal)) => map(pal) { case (_, l) => (a, l) }
        case Right((pa, al)) => Now((al._1, pa :: al._2))
      }
    }
  }

  def choose[A, B](pa: Par[A], pb: Par[B]): Par[Either[(A, Par[B]), (Par[A], B)]] = {
    Async { cb =>
      val won = new AtomicBoolean(false)

      val (ra, resultA, listenerA) = residual(pa)
      val (rb, resultB, listenerB) = residual(pb)

      runAsync(pa) { a =>
        resultA.set(a)

        if (won.compareAndSet(false, true)) cb(Left((a, rb)))
        else {}

        if (listenerA.compareAndSet(null, _ => ())) {}
        else listenerA.get.apply(a)
      }

      runAsync(pb) { b =>
        resultB.set(b)

        if (won.compareAndSet(false, true)) cb(Right((ra, b)))
        else {}

        if (listenerB.compareAndSet(null, _ => sys.error("unreachable"))) {}
        else listenerB.get.apply(b)
      }
    }
  }

  private def residual[A](p: Par[A]): (Par[A], AtomicReference[A], AtomicReference[A => Unit]) = {
    val used = new AtomicBoolean(false)
    val result = new AtomicReference[A]
    val listener = new AtomicReference[A => Unit](null)
    val residual = Async { (cb: A => Unit) =>
      if (used.compareAndSet(false, true)) {
        if (listener.compareAndSet(null, cb)) {}
        else cb(result.get)
      }
      else runAsync(p)(cb)
    }
    (residual, result, listener)
  }

  def chooseAnyStandalone[A](as: List[Par[A]]): Option[Par[(A, List[Par[A]])]] = as match {
    case Nil => None
    case _ => Some { Async { cb =>
      val won = new AtomicBoolean(false)
    
      val pars = as.zipWithIndex.map { case (p, ind) =>
        val used = new AtomicBoolean(false)
        val ref = new AtomicReference[A]
        val listener = new AtomicReference[A => Unit](null)
        val residual = Async { (cb: A => Unit) =>
          if (used.compareAndSet(false, true)) {
            if (listener.compareAndSet(null, cb)) {}
            else cb(ref.get)
          }
          else runAsync(p)(cb)
        }
        (ind, p, residual, listener, ref)
      }

      pars.foreach { case (ind, p, _, listener, ref) =>
        runAsync(p) { a =>
          ref.set(a)

          if (won.compareAndSet(false, true)) cb((a, pars.collect { case (i, _, rf, _, _) if i != ind => rf }))
          else {}

          if (listener.compareAndSet(null, _ => ())) {}
          else listener.get.apply(a)
        }
      }
    }}
  }

  def chooseViaChooseAny[A, B](pa: Par[A], pb: Par[B]): Par[Either[(A, Par[B]), (Par[A], B)]] =
    map(chooseAnyStandalone(List[Par[Either[A, B]]](map(pa)(Left(_)), map(pb)(Right(_)))).get) {
      (x: (Either[A, B], List[Par[Either[A, B]]])) => x match {
        case (Left(a), h :: _) => Left((a, map(h) {
          case Right(b) => b
          case _ => sys.error("error")
        }))
        case (Right(b), h :: _) => Right((map(h) {
          case Left(a) => a
          case _ => sys.error("error")
        }, b))
        case _ => sys.error("error")
      }
    }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = as.foldRight(now(List[A]())) { map2(_, _) { _ :: _ } }

  def sequenceR[A](as: List[Par[A]])(implicit es: ExecutorService = ExecutionContext.defaultExecutorService): Par[List[A]] = as match {
    case Nil => now(Nil)
    case h :: t => map2(h, fork(sequenceR(t)))(_ :: _)
  }

  def sequenceB[A](as: IndexedSeq[Par[A]])(implicit es: ExecutorService = ExecutionContext.defaultExecutorService): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) now(Vector()) 
    else if (as.length == 1) map(as.head)(Vector(_))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceB(l), sequenceB(r))(_ ++ _)
    }
  }

  def parMap[A, B](as: List[A])(f: A => B)(implicit es: ExecutorService = ExecutionContext.defaultExecutorService): Par[List[B]] = fork {
    sequence(as map asyncF(f))
  }

  def parFilter[A](as: List[A])(f: A => Boolean)(implicit es: ExecutorService = ExecutionContext.defaultExecutorService): Par[List[A]] = fork {
    val pars: List[Par[Option[A]]] = as map (asyncF(a => if (f(a)) Some(a) else None))
    map(sequence(pars))(_.flatten)
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond) { if (_) t else f }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n) { choices(_) }
  
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key) { choices(_) }

  def equal[A](a1: Par[A], a2: Par[A]): Boolean = run(a1) == run(a2)

  @annotation.tailrec
  final def step[A](pa: Par[A]): Par[A] = pa match {
    case Delay(t) => step(t())
    case Chain(Now(a), f) => step(f(a))
    case Chain(Delay(t), f) => step(t() flatMap f)
    case Chain(Chain(x, f), g) => step(x flatMap (f andThen g))
    case _ => pa
  }

  def runAsync[A](pa: Par[A])(cb: A => Unit): Unit = step(pa) match {
    case Now(a) => cb(a)
    case Async(k) => k(cb)
    case Chain(Async(k), f) => k { a => runAsync(f(a))(cb) }
    case _ => sys.error("Impossible since `step` eliminate these cases.")
  }

  def run[A](pa: Par[A]): A = pa match {
    case Now(a) => a
    case _ => {
      val latch = new java.util.concurrent.CountDownLatch(1)
      @volatile var result: Option[A] = None
      runAsync(pa) { a => result = Some(a); latch.countDown }
      latch.await
      result.get
    }
  }

  object syntax extends ParSyntax
}

object ExecutionContext {
  private val defaultDaemonThreadFactory: ThreadFactory = new ThreadFactory {
    val defaultThreadFactory = Executors.defaultThreadFactory()
    def newThread(r: Runnable) = {
      val t = defaultThreadFactory.newThread(r)
      t.setDaemon(true)
      t
    }
  }

  val defaultExecutorService: ExecutorService = {
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors, defaultDaemonThreadFactory)
  }
}

trait ParSyntax {
  implicit class ParOps[A](self: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(self)(f)
    def map2[B, C](other: Par[B])(f: (A, B) => C): Par[C] = Par.map2(self, other)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(self)(f)
    def equal(other: Par[A]): Boolean = Par.equal(self, other)
    def run: A = Par.run(self)
  }
}
