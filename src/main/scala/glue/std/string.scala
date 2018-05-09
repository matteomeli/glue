package glue
package std

import glue.typeclass.Monoid

object string extends StringImplicits

trait StringImplicits {
  implicit val stringIsMonoid: Monoid[String] = new Monoid[String] {
    val unit: String = ""
    def combine(a1: String, a2: String): String = a1 + a2
  }
}
