package glue.tests

import org.scalacheck.Properties

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

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
}