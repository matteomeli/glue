package glue
package concurrent

import java.util.concurrent.ExecutorService

sealed trait Par[A] {
  def map[B](f: A => B): Par[B] = Par.map(this)(f)
  def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(this)(f)
  def runAsync(cb: A => Unit): Unit = Par.runAsync(this)(cb)
  def run: A = Par.run(this)
}
case class Now[A](value: A) extends Par[A]
case class Delay[A](t: () => Par[A]) extends Par[A]
case class Async[A](k: (A => Unit) => Unit) extends Par[A]
case class Chain[A, B](pa: Par[A], f: A => Par[B]) extends Par[B]

object Par {
  def apply[A](a: => A)(implicit es: ExecutorService): Par[A] = Async { cb =>
    es.execute(new Runnable {
      def run = cb(a)
    })
  }

  def now[A](a: A): Par[A] = Now(a)

  def delayNow[A](a: => A): Par[A] = delay(now(a))

  def lazyNow[A](a: => A)(implicit es: ExecutorService): Par[A] = fork(now(a))

  def async[A](k: (A => Unit) => Unit): Par[A] = Async(k)

  def delay[A](pa: => Par[A]): Par[A] = Delay(() => pa)

  def asyncF[A, B](f: A => B)(implicit es: ExecutorService): A => Par[B] = a => lazyNow(f(a))

  def fork[A](pa: => Par[A])(implicit es: ExecutorService): Par[A] = join(Par(pa))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = flatMap(pa)(f andThen (now(_)))

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = Chain(pa, f)

  def join[A](pa: Par[Par[A]]): Par[A] = flatMap(pa)(identity)

  // TODO
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
/*
    es => {
      implicit val ctx = ExecutionContext.fromExecutorService(es)
      a(es).zipWith(b(es)) { (a, b) => f(a, b) }
    }
*/

  def sequence[A](as: List[Par[A]]): Par[List[A]] = as.foldRight(now(List[A]())) { map2(_, _) { _ :: _ } }

  def sequenceR[A](as: List[Par[A]])(implicit es: ExecutorService): Par[List[A]] = as match {
    case Nil => now(Nil)
    case h :: t => map2(h, fork(sequenceR(t)))(_ :: _)
  }

  def sequenceB[A](as: IndexedSeq[Par[A]])(implicit es: ExecutorService): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) now(Vector()) 
    else if (as.length == 1) map(as.head)(Vector(_))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceB(l), sequenceB(r))(_ ++ _)
    }
  }

  def parMap[A, B](as: List[A])(f: A => B)(implicit es: ExecutorService): Par[List[B]] = fork {
    sequence(as map asyncF(f))
  }

  def parFilter[A](as: List[A])(f: A => Boolean)(implicit es: ExecutorService): Par[List[A]] = fork {
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

trait ParSyntax {
  implicit class ParOps[A](self: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(self)(f)
    def map2[B, C](other: Par[B])(f: (A, B) => C): Par[C] = Par.map2(self, other)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(self)(f)
    def equal(other: Par[A]): Boolean = Par.equal(self, other)
    def run: A = Par.run(self)
  }
}
