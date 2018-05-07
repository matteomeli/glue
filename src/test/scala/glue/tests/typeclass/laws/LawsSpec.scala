package glue.tests.typeclass.laws

import glue.tests.GlueSpec
import glue.tests.GlueArbitrary._

import glue._
import glue.all._

import glue.NaturalTransformation._

class LawsSpec extends GlueSpec {
  checkAll("Monoid[String]", MonoidLawsSpec[String].monoid)
  checkAll("Monoid[List[Int]]", MonoidLawsSpec[List[Int]].monoid)
  checkAll("Monoid[List[String]]", MonoidLawsSpec[List[String]].monoid)
  checkAll("Monoid[Option[Int]]", MonoidLawsSpec[Option[Int]].monoid)
  checkAll("Monoid[Option[String]]", MonoidLawsSpec[Option[String]].monoid)
  checkAll("Monoid[Option[Int => Int]]", MonoidLawsSpec[Option[Int => Int]].monoid)
  checkAll("Monoid[Option[String => String]]", MonoidLawsSpec[Option[String => String]].monoid)

  checkAll("Traverse[Identity[Int]] Identity ~> Identity", TraverseLawsSpec[Identity].traverse[Identity, Identity, Int, Int, Int](id))
  checkAll("Traverse[Identity[String]] List ~> Option", TraverseLawsSpec[Identity].traverse[List, Option, String, String, String](headOption))
  checkAll("Traverse[Identity[Int]] Option ~> List", TraverseLawsSpec[Identity].traverse[Option, List, Int, Int, Int](optionList))

  checkAll("Monad[Identity[Int]]", MonadLawsSpec[Identity].monad[Int, Int, Int, Int])
  checkAll("Monad[List[Int]]", MonadLawsSpec[List].monad[Int, Int, Int, Int])
  checkAll("Monad[Option[Int]]", MonadLawsSpec[Option].monad[Int, Int, Int, Int])
  checkAll("Monad[Either[Int, Int]]", MonadLawsSpec[({type f[x] = Either[Int, x]})#f].monad[Int, Int, Int, Int])
}
