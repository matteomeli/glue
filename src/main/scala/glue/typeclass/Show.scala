package glue.typeclass

import scala.language.implicitConversions

trait Show[A] {
  def show(a: A): String
}

object Show extends ShowFunctions {
  def apply[A](implicit S: Show[A]): Show[A] = S

  def show[A](f: A => String): Show[A] = new Show[A] {
    def show(a: A): String = f(a)
  }

  def fromToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString
  }

  object syntax extends ShowSyntax

  object instances extends ShowInstances
}

trait ShowFunctions {
  def show[A: Show](a: A): String = Show[A].show(a)
}

object ShowStringContext {
  final case class InterpolateWithShow(override val toString: String) extends AnyVal
  object InterpolateWithShow {
    implicit def apply[A](a: A)(implicit S: Show[A]): InterpolateWithShow = 
      InterpolateWithShow(S.show(a))
  }
  final case class ShowInterpolator(val sc: StringContext) extends AnyVal {
    def show(args: InterpolateWithShow*): String = sc.s(args: _*)
  }
}

trait ShowSyntax {
  implicit class ShowOps[A: Show](self: A) {
    def show = Show[A].show(self)
  }

  implicit final def showInterpolator(sc: StringContext): ShowStringContext.ShowInterpolator =
    ShowStringContext.ShowInterpolator(sc)
}

trait ShowInstances {
  implicit val boolCanShow: Show[Boolean] = bool => s"bool $bool"
  implicit val byteCanShow: Show[Byte] = byte => s"byte $byte"
  implicit val charCanShow: Show[Char] = char => s"char $char"
  implicit val shortCanShow: Show[Short] = short => s"short $short"
  implicit val intCanShow: Show[Int] = int => s"int $int"
  implicit val bigIntCanShow: Show[BigInt] = bigInt => s"big int $bigInt"
  implicit val longCanShow: Show[Long] = long => s"long $long"
  implicit val floatCanShow: Show[Float] = float => s"float $float"
  implicit val doubleCanShow: Show[Double] = double => s"double $double"
  implicit val bigDecimalCanShow: Show[BigDecimal] = bigDecimal => s"big decimal $bigDecimal"
  implicit val stringCanShow: Show[String] = str => s"string $str"
}
