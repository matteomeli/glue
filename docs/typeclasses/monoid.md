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

val s: String = s1.combine(s2)
s.combine(unit[String])
```
