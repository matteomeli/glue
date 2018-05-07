package glue.tests

import org.scalacheck.Properties

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import glue.NaturalTransformation

abstract class GlueSpec extends FunSuite with Checkers {
  def checkAll(name: String, props: Properties): Unit =
    for ((propName, prop) <- props.properties) {
      test(name + "." + propName) {
        check(prop)
      }
    }

  def checkAll(props: Properties): Unit =
    for ((name, prop) <- props.properties) {
      test(name) {
        check(prop)
      }
    }


  // Some natural transformations
  val headOption: NaturalTransformation[List, Option] =
    new NaturalTransformation[List, Option] { def apply[A](as: List[A]): Option[A] = as.headOption }
  val optionList: NaturalTransformation[Option, List] =
    new NaturalTransformation[Option, List] { def apply[A](oa: Option[A]): List[A] = oa.toList }
}