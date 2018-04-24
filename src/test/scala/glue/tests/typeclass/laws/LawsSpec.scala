package glue.tests.typeclass.laws

import glue.tests.GlueSpec

import glue.prelude._

class LawsSpec extends GlueSpec {
  checkAll("Monoid[String]", MonoidLawsSpec[String].props)
  checkAll("Monoid[List[Int]]", MonoidLawsSpec[List[Int]].props)
  checkAll("Monoid[List[String]]", MonoidLawsSpec[List[String]].props)
  checkAll("Monoid[Option[Int]]", MonoidLawsSpec[Option[Int]].props)
  checkAll("Monoid[Option[String]]", MonoidLawsSpec[Option[String]].props)

  import glue.Id
  checkAll("Id[Int]", FunctorLawsSpec[Id].props[Int, Int, Int])
  checkAll("Id[String]", FunctorLawsSpec[Id].props[String, String, String])
  checkAll("List[Int]", FunctorLawsSpec[List].props[Int, Int, Int])
  checkAll("List[String]", FunctorLawsSpec[List].props[String, String, String])
  checkAll("Option[Int]", FunctorLawsSpec[Option].props[Int, Int, Int])
  checkAll("Option[String]", FunctorLawsSpec[Option].props[String, String, String])
  checkAll("Either[Int, Int]", FunctorLawsSpec[({type λ[α] = Either[Int, α]})#λ].props[Int, Int, Int])
  checkAll("Either[String, String]", FunctorLawsSpec[({type λ[α] = Either[String, α]})#λ].props[String, String, String])
}