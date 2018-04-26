package glue.std

import glue.typeclass.Monoid

object string extends StringFunctions with StringSyntax with StringInstances

trait StringFunctions {}

trait StringSyntax {}

trait StringInstances {
  implicit val stringIsMonoid: Monoid[String] = new Monoid[String] {
    val unit: String = ""
    def combine(a1: String, a2: String): String = a1 + a2
  }
}
