package glue.tests.typeclass

import glue.Show
import glue.Show._
import glue.Show.syntax._

import org.scalatest.{Matchers, WordSpec}

class ShowSpec extends WordSpec with Matchers {
  case class Cat(name: String)
  object Cat {
    implicit val catCanShow: Show[Cat] = tom => tom.name
  }

  class Mouse(val name: String) { override val toString: String = s"Mouse($name)" }
  object Mouse {
    implicit val mouseCanShow: Show[Mouse] = fromToString[Mouse]
  }

  sealed trait Room
  case object Kitchen extends Room
  object Room {
    implicit val locationCanShow: Show[Room] = Show.show {
      case Kitchen => "kitchen"
    }
  }

  val tom: Cat = Cat("Tom")
  val jerry: Mouse = new Mouse("Jerry")
  val kitchen: Room = Kitchen

  "An instance of Show" should {
    "show" in {
      tom.show shouldBe "Tom"
    }

    "show using toString when created with fromToString" in {
      jerry.show shouldBe jerry.toString
    }

    "be interpolated in the show string interpolator" in {
      show"$tom and $jerry are in the $kitchen" shouldBe s"Tom and ${jerry.toString} are in the kitchen"
    }
  }
}