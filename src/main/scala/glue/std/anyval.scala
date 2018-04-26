package glue.std

import glue.typeclass.Show

object anyval extends AnyValInstances

trait AnyValInstances {
  implicit val unitCanShow: Show[Unit] = Unit => "unit"
  implicit val boolCanShow: Show[Boolean] = bool => s"bool $bool"
  implicit val byteCanShow: Show[Byte] = byte => s"byte $byte"
  implicit val charCanShow: Show[Char] = char => s"char $char"
  implicit val shortCanShow: Show[Short] = short => s"short $short"
  implicit val intCanShow: Show[Int] = int => s"int $int"
  implicit val longCanShow: Show[Long] = long => s"long $long"
  implicit val floatCanShow: Show[Float] = float => s"float $float"
  implicit val doubleCanShow: Show[Double] = double => s"double $double"
  implicit val bigIntCanShow: Show[BigInt] = bigInt => s"bigint $bigInt"
  implicit val bigDecimalCanShow: Show[BigDecimal] = bigDecimal => s"bigdecimal $bigDecimal"
  implicit val stringCanShow: Show[String] = str => s"string $str"
}