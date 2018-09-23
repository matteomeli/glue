package glue
package effect

sealed trait ST[S, A] { self =>
  protected def run(s: S): (S, A)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S): (S, B) = {
      val (s1, a) = self.run(s)
      (s1, f(a))
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S): (S, B) = {
      val (s1, a) = self.run(s)
      f(a).run(s1)
    }
  }

  def apply[B](stf: ST[S, A => B]): ST[S, B] = new ST[S, B] {
    def run(s: S): (S, B) = {
      val (s1, a) = self.run(s)
      val (s2, f) = stf.run(s1)
      (s2, f(a))
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo: A = a
    new ST[S, A] {
      def run(s: S): (S, A) = (s, memo)
    }
  }

  def runST[A](str: STRunnable[A]): A = str.apply[Unit].run(())._2

  def noop[S] = ST[S, Unit](())

  object implicits extends STImplicits
}

trait STImplicits {
  private implicit def STisFunctor[S]: Functor[({ type f[x] = ST[S, x]})#f] = new Functor[({ type f[x] = ST[S, x]})#f] {
    def map[A, B](sta: ST[S, A])(f: A => B): ST[S, B] = sta map f
  }

  private implicit def STisAppicative[S]: Applicative[({ type f[x] = ST[S, x]})#f] = new Applicative[({ type f[x] = ST[S, x]})#f] {
    val functor: Functor[({ type f[x] = ST[S, x]})#f] = Functor[({ type f[x] = ST[S, x]})#f]
    def apply[A, B](f: ST[S, A => B])(sta: ST[S, A]): ST[S, B] = sta apply f
    def pure[A](a: => A): ST[S, A] = ST(a)
  }

  implicit def STisMonad[S]: Monad[({ type f[x] = ST[S, x]})#f] = new Monad[({ type f[x] = ST[S, x]})#f] {
    val applicative: Applicative[({ type f[x] = ST[S, x]})#f] = Applicative[({ type f[x] = ST[S, x]})#f]
    def flatMap[A, B](sta: ST[S, A])(f: A => ST[S, B]): ST[S, B] = sta flatMap f
  }
}

trait STRunnable[A] {
  def apply[S]: ST[S, A]
}

sealed trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)
  
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (S, Unit) = {
      cell = a
      (s, ())
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(new STRef[S, A] { var cell = a })
}

sealed abstract class STArray[S, A: Manifest] {
  protected val array: Array[A]

  def length: ST[S, Int] = ST(array.size)

  def read(index: Int): ST[S, A] = new ST[S, A] {
    def run(s: S): (S, A) = {
      (s, array(index))
    }
  }

  def write(index: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (S, Unit) = {
      array(index) = a
      (s, ())
    }
  }

  def toList: ST[S, List[A]] = ST(array.toList)

  def fill(m: Map[Int, A]): ST[S, Unit] = {
    m.foldLeft(ST.noop[S]) { case (st, (k, v)) => st flatMap { _ => write(k, v) } }
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(j, x)
    _ <- write(i, y)
  } yield ()
}

object STArray {
  def apply[S, A: Manifest](size: Int): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val array = new Array[A](size)
  })

  def apply[S, A: Manifest](size: Int, a: A): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val array = Array.fill(size)(a)
  })

  def apply[S, A: Manifest](list: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val array = list.toArray
  })

  def apply[S, A: Manifest](args: A*): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val array = args.toArray
  })
}

object Quicksort {
  def partition[S](array: STArray[S, Int], l: Int, r: Int, pivot: Int): ST[S, Int] = for {
    vp <- array.read(pivot)
    _ <- array.swap(pivot, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(ST.noop[S]) {
      (st, i) => for {
        _ <- st
        vi <- array.read(i)
        _ <- if (vi < vp) for {
          vj <- j.read
          _ <- array.swap(i, vj)
          _ <- j.write(vj + 1)
        } yield () else ST.noop[S]
      } yield ()
    }
    x <- j.read
    _ <- array.swap(x, r)
  } yield x

  def qs[S](array: STArray[S, Int], l: Int, r: Int): ST[S, Unit] = if (l < r) for {
    pi <- partition(array, l, r, l + (r - l) / 2)
    _ <- qs(array, l, pi - 1)
    _ <- qs(array, pi + 1, r)
  } yield () else ST.noop[S]

  def quicksort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty) xs else ST.runST(new STRunnable[List[Int]] {
      def apply[S]: ST[S, List[Int]] = for {
        array <- STArray(xs)
        length <- array.length
        _ <- qs(array, 0, length - 1)
        sorted <- array.toList
      } yield sorted
    })
  }
}
