package glue.tests.typeclass.laws

import glue.tests.GlueSpec
import glue.tests.GlueArbitrary._

import glue._
import glue.all._

class LawsSpec extends GlueSpec {
  checkAll("Monoid[String]", MonoidLawsSpec[String].monoid)
  checkAll("Monoid[List[Int]]", MonoidLawsSpec[List[Int]].monoid)
  checkAll("Monoid[List[String]]", MonoidLawsSpec[List[String]].monoid)
  checkAll("Monoid[Option[Int]]", MonoidLawsSpec[Option[Int]].monoid)
  checkAll("Monoid[Option[String]]", MonoidLawsSpec[Option[String]].monoid)
  checkAll("Monoid[Option[Int => Int]]", MonoidLawsSpec[Option[Int => Int]].monoid)
  checkAll("Monoid[Option[String => String]]", MonoidLawsSpec[Option[String => String]].monoid)

  checkAll("Identity[Int]", FunctorLawsSpec[Identity].functor[Int, Int, Int])
  checkAll("Identity[String]", FunctorLawsSpec[Identity].functor[String, String, String])
  checkAll("List[Int]", FunctorLawsSpec[List].functor[Int, Int, Int])
  checkAll("List[String]", FunctorLawsSpec[List].functor[String, String, String])
  checkAll("Option[Int]", FunctorLawsSpec[Option].functor[Int, Int, Int])
  checkAll("Option[String]", FunctorLawsSpec[Option].functor[String, String, String])
  checkAll("Either[Int, Int]", FunctorLawsSpec[({type f[x] = Either[Int, x]})#f].functor[Int, Int, Int])
  checkAll("Either[String, String]", FunctorLawsSpec[({type f[x] = Either[String, x]})#f].functor[String, String, String])

  checkAll("Identity[Int]", ApplicativeLawsSpec[Identity].applicative[Int, Int, Int, Int])
  checkAll("Identity[String]", ApplicativeLawsSpec[Identity].applicative[String, String, String, String])
  checkAll("List[Int]", ApplicativeLawsSpec[List].applicative[Int, Int, Int, Int])
  checkAll("List[String]", ApplicativeLawsSpec[List].applicative[String, String, String, String])
  checkAll("Option[Int]", ApplicativeLawsSpec[Option].applicative[Int, Int, Int, Int])
  checkAll("Option[String]", ApplicativeLawsSpec[Option].applicative[String, String, String, String])
  checkAll("Either[Int, Int]", ApplicativeLawsSpec[({type f[x] = Either[Int, x]})#f].applicative[Int, Int, Int, Int])
  checkAll("Either[String, String]", ApplicativeLawsSpec[({type f[x] = Either[String, x]})#f].applicative[String, String, String, String])

  checkAll("Identity[Int] (Applicative[Identity])", TraverseLawsSpec[Identity].traverse[Identity, Identity, Int, Int, Int])
  checkAll("Identity[Int] (Applicative[List], Applicative[Option])", TraverseLawsSpec[Identity].traverse[List, Option, Int, Int, Int])
  checkAll("Identity[Int] (Applicative[Option], Applicative[List])", TraverseLawsSpec[Identity].traverse[Option, List, Int, Int, Int])
  // TODO: Add more tests for other traversable with different combinations of applicatives
}
