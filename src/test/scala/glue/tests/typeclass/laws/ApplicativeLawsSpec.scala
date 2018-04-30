package glue.tests.typeclass.laws

import glue.Applicative
import glue.typeclass.ApplicativeLaws

import org.scalacheck.{Arbitrary, Cogen, Prop, Properties}
import Prop._

trait ApplicativeLawsSpec[F[_]] {
  def laws: ApplicativeLaws[F]

  def applicative[A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary](
    implicit arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    arbFAB: Arbitrary[F[A => B]],
    cogenA: Cogen[A],
    cogenB: Cogen[B],
    cogenC: Cogen[C]): Properties = new Properties("applicative") {
      // Any Applicative is also a Functor, so Functor laws should be respected by any Applicative
      include(FunctorLawsSpec[F](laws.applicative.functor).functor[A, B, C])
      property("leftIdentity") = forAll(laws.leftIdentity[A] _)
      property("rightIdentity") = forAll(laws.rightIdentity[A] _)
      property("associativity") = forAll(laws.associativity[A, B, C] _)
      property("homomorphism") = forAll(laws.homomorphism[A, B] _)
      property("interchange") = forAll(laws.interchange[A, B] _)
      property("applicativeMap") = forAll(laws.applicativeMap[A, B] _)
      property("naturality") = forAll(laws.naturality[A, B, C, D] _)
  }
}

object ApplicativeLawsSpec {
  def apply[F[_]: Applicative]: ApplicativeLawsSpec[F] =
    new ApplicativeLawsSpec[F] { def laws: ApplicativeLaws[F] = ApplicativeLaws[F] }
}
