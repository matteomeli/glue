package glue.tests.typeclass.laws

import glue.tests.GlueSpec
import glue.tests.GlueArbitrary._

import glue._
import glue.all._

class LawsSpec extends GlueSpec {
  //checkAll("Monoid[String]", MonoidLawsSpec[String].monoid)
  //checkAll("Monoid[List[Int]]", MonoidLawsSpec[List[Int]].monoid)
  //checkAll("Monoid[List[String]]", MonoidLawsSpec[List[String]].monoid)
  //checkAll("Monoid[Option[Int]]", MonoidLawsSpec[Option[Int]].monoid)
  //checkAll("Monoid[Option[String]]", MonoidLawsSpec[Option[String]].monoid)
  //checkAll("Monoid[Option[Int => Int]]", MonoidLawsSpec[Option[Int => Int]].monoid)
  //checkAll("Monoid[Option[String => String]]", MonoidLawsSpec[Option[String => String]].monoid)

  checkAll("Identity[Int]", FunctorLawsSpec[Identity].functor[Int, Int, Int])
  //checkAll("Id[String]", FunctorLawsSpec[Id].functor[String, String, String])
  //checkAll("List[Int]", FunctorLawsSpec[List].functor[Int, Int, Int])
  //checkAll("List[String]", FunctorLawsSpec[List].functor[String, String, String])
  //checkAll("Option[Int]", FunctorLawsSpec[Option].functor[Int, Int, Int])
  //checkAll("Option[String]", FunctorLawsSpec[Option].functor[String, String, String])
  //checkAll("Either[Int, Int]", FunctorLawsSpec[({type λ[α] = Either[Int, α]})#λ].functor[Int, Int, Int])
  //checkAll("Either[String, String]", FunctorLawsSpec[({type λ[α] = Either[String, α]})#λ].functor[String, String, String])

  checkAll("Identity[Int]", ApplicativeLawsSpec[Identity].applicative[Int, Int, Int, Int])
  checkAll("Identity[String]", ApplicativeLawsSpec[Identity].applicative[String, String, String, String])
  checkAll("List[Int]", ApplicativeLawsSpec[List].applicative[Int, Int, Int, Int])
  checkAll("List[String]", ApplicativeLawsSpec[List].applicative[String, String, String, String])
  checkAll("Option[Int]", ApplicativeLawsSpec[Option].applicative[Int, Int, Int, Int])
  checkAll("Option[String]", ApplicativeLawsSpec[Option].applicative[String, String, String, String])
  checkAll("Either[Int, Int]", ApplicativeLawsSpec[({type λ[α] = Either[Int, α]})#λ].applicative[Int, Int, Int, Int])
  checkAll("Either[String, String]", ApplicativeLawsSpec[({type λ[α] = Either[String, α]})#λ].applicative[String, String, String, String])
}
