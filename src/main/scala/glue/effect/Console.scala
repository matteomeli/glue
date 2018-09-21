package glue
package effect

import scala.io.StdIn.readLine

sealed trait Console[A]
final object ReadLine extends Console[Option[String]]
final case class WriteLine(line: String) extends Console[Unit]

object Console {
  type ConsoleIO[A] = Free[Console, A]
  
  def readLn: ConsoleIO[Option[String]] = Effect(ReadLine)
  def writeLn(line: String): ConsoleIO[Unit] = Effect(WriteLine(line))

  // Interprets a Console `program` as a Function0
  val toFunction0: NaturalTransformation[Console, Function0] =
    new NaturalTransformation[Console, Function0] {
      def apply[A](c: Console[A]): Function0[A] = () => c match {
        case ReadLine =>
          try Some(readLine())
          catch { case _: Exception => None }
        case WriteLine(line) => println(line)
      }
    }

  def runConsole[A](c: ConsoleIO[A]): A = Free.translate(c)(toFunction0).run

  type ConsoleReader[A] = Reader[String, A]
  val ConsoleReader = Reader
  // An interpreter that translates a Console `program` to a Reader
  val toReader: NaturalTransformation[Console, ConsoleReader] =
    new NaturalTransformation[Console, ConsoleReader] {
      def apply[A](ca: Console[A]): ConsoleReader[A] = ConsoleReader { s => 
        ca match {
          case ReadLine => Some(s)
          case WriteLine(_) => () 
        }
      }
    }

  import data.Reader.implicits._
  def runConsoleReader[A](c: ConsoleIO[A]): ConsoleReader[A] =
    Free.runFree(c)(toReader)

  case class Buffers(in: List[String], out: List[String])
  type ConsoleState[A] = State[Buffers, A]
  val ConsoleState = State
  // An interpreter to convert a Console `program` to a State[Buffer, A]
  val toState: NaturalTransformation[Console, ConsoleState] =
    new NaturalTransformation[Console, ConsoleState] {
      def apply[A](ca: Console[A]): ConsoleState[A] = ConsoleState { buffers =>
        ca match {
          case ReadLine => buffers.in match {
            case List() => (buffers, None)
            case h :: t => (buffers.copy(in = t), Some(h))
          }
          case WriteLine(l) => (buffers.copy(out = buffers.out :+ l), ())
        }
      }
    }

  import data.State.implicits._
  def runConsoleState[A](c: ConsoleIO[A]): ConsoleState[A] =
    Free.runFree(c)(toState)
}