package glue
package concurrent

import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration.Duration

import java.util.concurrent.ExecutorService

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => Future.successful(a)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    implicit val ctx = ExecutionContext.fromExecutorService(es)
    a(es).zipWith(b(es)) { (a, b) => f(a, b) }
  }

  def map[A, B](a: Par[A])(f: A => B): Par[B] = es => {
    implicit val ctx = ExecutionContext.fromExecutorService(es)
    a(es) map f
  }

  // TODO: This doesn't really fork computation 'a' in a separate logical thread,
  // as soon as 'ec' is passed a executes the future into the main thread
  def fork[A](a: => Par[A]): Par[A] = es => a(es)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(as map asyncF(f))
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = as.foldRight(unit(List[A]())) { map2(_, _) { _ :: _ } }

  def sequenceR[A](as: List[Par[A]]): Par[List[A]] = as match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequenceR(t)))(_ :: _)
  }

  def sequenceB[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector()) 
    else if (as.length == 1) map(as.head)(Vector(_))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceB(l), sequenceB(r))(_ ++ _)
    }
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val pars: List[Par[Option[A]]] = as map (asyncF(a => if (f(a)) Some(a) else None))
    map(sequence(pars))(_.flatten)
  }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => {
      implicit val ctx = ExecutionContext.fromExecutorService(es)
      pa(es) flatMap { a => f(a)(es) }
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond) { if (_) t else f }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(n) { choices(_) }
  
  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(key) { choices(_) }

  def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)

  def equal[A](a1: Par[A], a2: Par[A])(implicit es: ExecutorService): Boolean =
    Await.result(map2(a1, a2)(_ == _)(es), Duration.Inf)

  def run[A](a: Par[A])(implicit es: ExecutorService): A = {
    implicit val ctx = ExecutionContext.fromExecutorService(es)
    val latch = new java.util.concurrent.CountDownLatch(1)
    @volatile var result: Option[A] = None
    a(es) foreach { a => result = Some(a); latch.countDown }
    latch.await
    result.get
  }

  object syntax extends ParSyntax
}

trait ParSyntax {
  import Par.Par

  implicit class ParOps[A](self: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(self)(f)
    def map2[B, C](other: Par[B])(f: (A, B) => C): Par[C] = Par.map2(self, other)(f)
    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(self)(f)
    def equal(other: Par[A])(implicit es: ExecutorService): Boolean = Par.equal(self, other)(es)
    def run(implicit es: ExecutorService): A = Par.run(self)(es)
  }
}