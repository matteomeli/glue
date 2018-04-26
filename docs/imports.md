# Imports

The simplest approach is to import everything:

```scala
import glue._
import glue.all._
```

The `glue._` import brings in all the available *type classes*, such as [Monoid](./typeclasses/Monoid.md). Instead of the all `glue` package, you can import only the type classes you need as well:

```scala
import glue.Monoid
import glue.Foldable
```

The `glue.all._` import does two things. Firstly, it brings in scope implicit type class instances - like `Monoid[String]`. Secondly, it adds additional syntax enhancement for those type classes using ad-hoc polymorphism:

```scala
// glue adds a show function to standard library type Int
val i: Int = 2
i.show
```

Finally, if you want to import à-la-carte instead, you can do so by importing only what you need for any type class, its functions, its instances and its syntax:

```scala
import glue.Monoid
import glue.Monoid._
import glue.Monoid.syntax._
import glue.Monoid.instances._
```
