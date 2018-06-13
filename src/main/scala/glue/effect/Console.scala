package glue
package effect

sealed trait Console[A]
final object ReadLine extends Console[Option[String]]
final case class WriteLine(line: String) extends Console[Unit]

object Console {
  /*import glue._
  import glue.all._
  import glue.effect._
  def ReadLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }
  import glue.io.IO.implicits._
  val p = Monad[IO].forever(PrintLine("Still going..."))
  p.run*/
}

trait ConsoleFunctions