package glue
package effect

sealed trait Console[A]
final object ReadLine extends Console[Option[String]]
final case class WriteLine(line: String) extends Console[Unit]

object Console {
  type ConsoleIO[A] = Free[Console, A]
  
  def readLine: ConsoleIO[Option[String]] = Effect(ReadLine)
  def writeLine(line: String): ConsoleIO[Unit] = Effect(WriteLine(line))
}