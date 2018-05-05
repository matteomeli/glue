package glue.tests.typeclass.laws

import glue.{Applicative, Traverse}
import glue.typeclass.TraverseLaws

import org.scalacheck.{Arbitrary, Cogen, Prop, Properties}
import Prop._

trait TraverseLawsSpec[F[_]] {
  def laws: TraverseLaws[F]

  def traverse[G[_]: Applicative, H[_]: Applicative, A: Arbitrary, B: Arbitrary, C: Arbitrary](
    implicit arbFA: Arbitrary[F[A]],
    f: Arbitrary[A => G[B]],
    g: Arbitrary[B => H[C]],
    cogenA: Cogen[A],
    cogenB: Cogen[B]): Properties = new Properties("traverse") {
      include(FunctorLawsSpec[F](laws.traversable.functor).functor[A, B, C])
      property("identity") = forAll(laws.identity[A] _)
      property("compositon") = forAll(laws.composition[G, H, A, B, C] _)
    }
}

object TraverseLawsSpec {
  def apply[F[_]: Traverse]: TraverseLawsSpec[F] =
    new TraverseLawsSpec[F] { def laws: TraverseLaws[F] = TraverseLaws[F] }
}
