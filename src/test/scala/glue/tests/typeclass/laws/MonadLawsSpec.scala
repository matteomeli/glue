package glue.tests.typeclass.laws

import glue.Monad
import glue.typeclass.MonadLaws

import org.scalacheck.{Arbitrary, Cogen, Prop, Properties}
import Prop._

trait MonadLawsSpec[F[_]] {
  def laws: MonadLaws[F]

  def monad[ A: Arbitrary, B: Arbitrary, C: Arbitrary, D: Arbitrary](
    implicit arbFA: Arbitrary[F[A]],
    arbFB: Arbitrary[F[B]],
    arbFC: Arbitrary[F[C]],
    arbFAB: Arbitrary[F[A => B]],
    f: Arbitrary[A => F[B]],
    g: Arbitrary[B => F[C]],
    cogenA: Cogen[A],
    cogenB: Cogen[B],
    cogenC: Cogen[C]): Properties = new Properties("monad") {
      // Any Monad is also an Applicative, so Applicative laws should be respected by any Monad
      include(ApplicativeLawsSpec[F](laws.monad.applicative).applicative[A, B, C, D])
      property("leftIdentity") = forAll(laws.leftIdentity[A, B] _)
      property("rightIdentity") = forAll(laws.rightIdentity[A] _)
      property("associativity") = forAll(laws.associativity[A, B, C] _)
      property("coherence") = forAll(laws.coherence[A, B] _)
    }
}

object MonadLawsSpec {
  def apply[F[_]: Monad]: MonadLawsSpec[F] =
    new MonadLawsSpec[F] { def laws: MonadLaws[F] = MonadLaws[F] }
}
