# Imports

The simplest approach is to import everything:

```scala
import glue._
import glue.preluse._
```

The `glue._` import brings in all the available *type classes*, such as [Monoid](./typeclasses/Monoid.md). Instead of the all `glue` package, you can import only the type classes you need as well:

```scala
import glue.Monoid
import glue.Foldable
```

The `glue.prelude._` import does two things. Firstly, it brings in scope implicit type class instances - like `Monoid[String]`. Secondly, it adds additional syntax enhancement for those type classes using ad-hoc polymorphism:

```scala
// glue adds a show function to standard library type Int
val i: Int = 2
i.show
```

Finally, if you wnat to import à-la-carte instead, you can do so by importing from `glue.typeclass` for the type class, the instances and the syntax you need:

```scala
import glue.typeclass.Monoid
import glue.typeclass.Monoid._
import glue.typeclass.Monoid.syntax._
```