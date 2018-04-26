# Foldable

A foldable data structure is able to be folded. Usually when we need to write code that processes data contained in a data structure, we often don't care about the shape of that structure.

Like if we have a list of integers and we want to calculate their sum, we could use foldRight:

```scala
ints.foldRight(0)(_ + _)
```

Looking at the above snippet, it's obvious we should not care about the data type of `ints`. It sould be a `List`, a `Vector`, a `Tree`, a `Stream` or any other custom data structure providing a `foldRight`-kind function.

## Instance definition

```scala
import glue.Foldable

implicit val listIsFoldable: Foldable[List] = new Foldable[List] {
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
  def foldMap[A, B](as: List[A])(f: A => B)(implicit M: Monoid[B]): B =
    as.foldLeft(M.unit)((b, a) => M.combine(b, f(a)))
}
```

## Usage

```scala
import glue._
import glue.all._

implicit val optionIsFoldable: Foldable[Option] = new Foldable[Option] {
  def foldLeft[A, B](oa: Option[A])(z: B)(f: (B, A) => B): B = oa.map(f(z, _)).getOrElse(z)
  def foldRight[A, B](oa: Option[A])(z: B)(f: (A, B) => B): B = oa.map(f(_, z)).getOrElse(z)
  def foldMap[A, B](oa: Option[A])(f: A => B)(implicit M: Monoid[B]): B =
    oa.map(a => M.combine(f(a), M.unit)).getOrElse(M.unit)
}

val os: Option[String] = Some("hello")
val i: Int = os.foldRight(0)((a, b) => b + a.length)  // i: Int = 5
```
