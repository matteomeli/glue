package glue.tests.typeclass.laws

import glue.Functor
import glue.typeclass.FunctorLaws

import org.scalacheck.{Arbitrary, Cogen, Prop, Properties}
import Prop._

trait FunctorLawsSpec[F[_]] {
  def laws: FunctorLaws[F]

  def functor[A: Arbitrary, B: Arbitrary, C: Arbitrary](
    implicit arbFA: Arbitrary[F[A]],
    cogenA: Cogen[A],
    cogenB: Cogen[B]): Properties = new Properties("functor") {
    property("identity") = forAll(laws.identity[A] _)
    property("composition") = forAll(laws.composition[A, B, C] _)
  }
}

object FunctorLawsSpec {
  def apply[F[_]: Functor]: FunctorLawsSpec[F] =
    new FunctorLawsSpec[F] { def laws: FunctorLaws[F] = FunctorLaws[F] }
}
