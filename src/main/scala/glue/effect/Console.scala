package glue
package effect

sealed trait Console[A]
final object ReadLine extends Console[Option[String]]
final case class WriteLine(line: String) extends Console[Unit]
