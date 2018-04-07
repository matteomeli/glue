# Show

Show provides conversion of values to readable strings. It is an alternative to the classic `toString` method. It is defined by the only function `show` that consumes a generic type A and produces a string representation of A.

## Instance definition

```scala
import glue.Show

implicit val intCanShow: Show[Int] = int => s"int $int"
```

## Usage

```scala
import glue._
import glue.prelude._

val i: Int = 0;
i.show  // "int 0"
1.show  // "int 1"
```
