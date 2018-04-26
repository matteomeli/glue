package glue.tests.typeclass.laws

import glue.Eq
import glue.typeclass.EqLaws

import org.scalacheck.{Arbitrary, Prop, Properties}
import Prop._

trait EqLawsSpec[A] {
  def laws: EqLaws[A]

  def props(implicit arbA: Arbitrary[A]): Properties = new Properties("eq") {
    property("simmetry") = forAll(laws.simmetry _)
    property("reflexivity") = forAll(laws.reflexivity _)
    property("transitivity") = forAll(laws.transitivity _)
  }
}

object EqLawsSpec {
  def apply[A: Eq]: EqLawsSpec[A] = new EqLawsSpec[A] { def laws: EqLaws[A] = EqLaws[A] }
}
