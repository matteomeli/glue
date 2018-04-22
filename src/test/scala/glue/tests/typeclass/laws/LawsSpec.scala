package glue.tests.typeclass.laws

import glue.tests.GlueSpec

import glue.prelude._

class LawsSpec extends GlueSpec {
  checkAll("Monoid[String]", MonoidLawsSpec[String].props)
  checkAll("Monoid[List[Int]]", MonoidLawsSpec[List[Int]].props)
  checkAll("Monoid[List[String]]", MonoidLawsSpec[List[String]].props)
  checkAll("Monoid[Option[Int]]", MonoidLawsSpec[Option[Int]].props)
  checkAll("Monoid[Option[String]]", MonoidLawsSpec[Option[String]].props)
}