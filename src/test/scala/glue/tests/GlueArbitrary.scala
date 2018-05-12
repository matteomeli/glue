package glue.tests

import glue._
import glue.all._

import org.scalacheck.{Arbitrary, Gen}
import Gen.const

object GlueArbitrary {
  private def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]

  implicit def identityArbitrary[A: Arbitrary]: Arbitrary[Identity[A]] =
    Functor[Arbitrary].map(arb[A])(Identity(_))

  implicit val arbitraryIsApplicative: Applicative[Arbitrary] = new Applicative[Arbitrary] {
    val functor: Functor[Arbitrary] = new Functor[Arbitrary] {
      def map[A, B](fa: Arbitrary[A])(f: A => B): Arbitrary[B] = Arbitrary(fa.arbitrary map f)
    }
    def pure[A](a: => A): Arbitrary[A] = Arbitrary(const(a))
    def apply[A, B](f: Arbitrary[A => B])(fa: Arbitrary[A]): Arbitrary[B] = Arbitrary(fa.arbitrary flatMap {
      a => f.arbitrary.map(_(a))
    })
  }
}