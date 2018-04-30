package glue.tests.typeclass.laws

import glue.Monoid
import glue.typeclass.MonoidLaws

import org.scalacheck.{Arbitrary, Prop, Properties}
import Prop._

trait MonoidLawsSpec[A] {
  def laws: MonoidLaws[A]

  def monoid(implicit arbA: Arbitrary[A]): Properties = new Properties("monoid") {
    property("leftIdentity") = forAll(laws.leftIdentity _)
    property("rightIdentity") = forAll(laws.rightIdentity _)
    property("associativity") = forAll(laws.associativity _)
  }
}

object MonoidLawsSpec {
  def apply[A: Monoid]: MonoidLawsSpec[A] =
    new MonoidLawsSpec[A] { def laws: MonoidLaws[A] = MonoidLaws[A] }
}
