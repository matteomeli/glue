# Monoid

A monoid is an algebraic structure with a single binary operation and a unique identity element. Thus, a monoid is a *semigroup* extended with an identity element.

A monoid instance must satisfy the following laws:
* Associativity of the binary operation: `combine(x, combine(y, z)) == combine(combine(x, y), z)`
* Left identity: `combine(unit, x) == x`
* Right identity: `combine(x, unit) == x`

## Instance definition

```scala
import glue.Monoid

implicit val intAdditionIsMonoid: Monoid[Int] = new Monoid[Int] {
  val unit: Int = 0
  def combine(i1: Int, i2: Int): Int = i1 + i2
}
```

## Usage

```scala
import glue._
import glue.prelude._

val s1: String = "Hello, "
val s2: String = "world!"

val s: String = s1.combine(s2)  // s: String =  "Hello, world!"
s.combine(unit[String]) // res1: String = "Hello, world!"


implicit val optionIsMonoid[A]: Monoid[Option] = new Monoid[Option] {
  val unit: Option[A] = None
  def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
}

val o1: Option[Int] = Some(1)
val o2: Option[Int] = None
o1.combine(o2)  // res1: Option[Int] = Some(1)
o2.combine(o1)  // res2: Option[Int] = Some(1)
o2.combine(o2)  // res3: Option[Int] = None
```
