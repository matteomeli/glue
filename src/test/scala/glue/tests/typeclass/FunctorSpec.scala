package glue.tests.typeclass

import glue.Functor
import glue.Functor._
import glue.Functor.syntax._
import glue.Functor.instances._

import glue.std.option._

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class FunctorSpec extends WordSpec with Matchers {
  val len: String => Int = _.length
  val double: Int => Int = (i: Int) => i * i

  "A Functor" should {
    "map (fmap)" in {
      some(1).fmap((1+)) shouldBe some(2)
      none[String].fmap(_.length) shouldBe none[String]

      List[Int]().fmap((1+)) shouldBe List[Int]()
      List(1, 2, 3).fmap((1+)) shouldBe List(2, 3, 4)

      List("hello", "world").fmap(_.length) shouldBe List(5, 5)
    }

    "replaceL" in {
      some("a").replaceL(1) shouldBe some(1)
      none[Int].replaceL("x") shouldBe none[Int]
    }

    "replaceR" in {
      replaceR(some(1), "a") shouldBe some("a")
      replaceR(none[String], 1) shouldBe none[String]
    }

    "as" in {
      some(2.0).as("2.0") shouldBe some("2.0")
      none[Int].as("z") shouldBe none[Int]

      List(1, 2, 3).as("a") shouldBe List("a", "a", "a")
    }

    "void" in {
      some(1).void shouldBe some(())
      none[String].void shouldBe none[String]

      List(1, 2, 3).void shouldBe List((), (), ())
    }

    "fcompose" in {
      some("glue").fcompose(len, double) shouldBe some(16)

      List("aaa", "bb", "c").fcompose(len, identity[Int]) shouldBe List(3, 2, 1)
    }

    "fproduct" in {
      List("aaa", "bb", "c").fproduct(len).toMap shouldBe Map("aaa" -> 3, "bb" -> 2, "c" -> 1)
    }

    "pair" in {
      some(1).pair shouldBe some((1, 1))
    }

    "strengthL" in {
      List(1, 2, 3).strengthL("a") shouldBe List("a" -> 1, "a" -> 2, "a" -> 3)
    }

    "strengthR" in {
      List(1, 2, 3).strengthR("a") shouldBe List(1 -> "a", 2 -> "a", 3 -> "a")
    }

    "lift" in {
      val lenOption: Option[String] => Option[Int] = lift(len)
      lenOption(some("abcd")) shouldBe Some(4)
    }

    "composeF" in {
      val listOption = Functor[List] compose Functor[Option]
      listOption.map(List(Some(1), None, Some(3)))(_ + 1) shouldBe List(Some(2), None, Some(4))
    }

    "productF" in {
      val optionPair = Functor[Option] product Functor[Option]
      optionPair.map((Some("aaa"), Some("bb")))(len) shouldBe ((Some(3), Some(2)))
    }
  }
}