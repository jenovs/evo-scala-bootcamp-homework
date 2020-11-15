package http

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.Blocker
import cats.syntax.all._

import scala.concurrent.ExecutionContext
import scala.util.{Random, Try}

import io.circe._
import scala.collection.mutable.ListBuffer

// import akka.http.javadsl.model.headers.Cookie

// Homework. Place the solution under `http` package in your homework repository.
//
// Write a server and a client that play a number guessing game together.
//
// Communication flow should be as follows:
// 1. The client asks the server to start a new game by providing the minimum and the maximum number that can
//    be guessed, as well as the maximum number of attempts.
// 2. The server comes up with some random number within the provided range.
// 3. The client starts guessing the number. Upon each attempt, the server evaluates the guess and responds to
//    the client, whether the current number is lower, greater or equal to the guessed one.
// 4. The game ends when the number is guessed or there are no more attempts left. At this point the client
//    should terminate, while the server may continue running forever.
// 5. The server should support playing many separate games (with different clients) at the same time.
//
// Use HTTP or WebSocket for communication. The exact protocol and message format to use is not specified and
// should be designed while working on the task.
object GuessServer extends IOApp {
  import org.http4s.server.blaze.BlazeServerBuilder
  import org.http4s.dsl.impl.QueryParamDecoderMatcher
  import org.http4s.client.dsl.io._
  import org.http4s.dsl.io._
  import org.http4s.implicits._
  import org.http4s.client.blaze.BlazeClientBuilder
  import org.http4s._

  import org.slf4j.LoggerFactory
  import ch.qos.logback.classic.{Level, Logger}

  LoggerFactory.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME).asInstanceOf[Logger].setLevel(Level.INFO)

  case class User private (id: String, number: Long, guessesLeft: Long)

  object User {
    def create =
      (min: Long, max: Long, guesses: Long) => {
        val id = java.util.UUID.randomUUID.toString
        val number = Random.between(min, max + 1)
        User(id, number, guesses)
      }
  }

  case class Db() {
    val data = collection.mutable.Map.empty[String, User]
    def addUser(user: User): Unit = {
      if (user.guessesLeft <= 0) this.removeUser(user.id)
      else data += user.id -> user
    }

    private def removeUser(id: String): Unit = {
      data.remove(id)
      ()
    }

    def guess(userId: String, guess: Long): Option[String] = {
      val user = data.get(userId)

      val result = for {
        u <- user
        if (u.guessesLeft > 0)
        n = u.number
        guessDiff = n - guess
        result <- guessDiff match {
          case 0 => {
            this.addUser(u.copy(guessesLeft = 0))
            Some("correct")
          }
          case _ if (u.guessesLeft <= 1) => {
            this.addUser(u.copy(guessesLeft = 0))
            Some(s"No more guesses left. The number was $n")
          }
          case d if (d > 0) => {
            this.addUser(u.copy(guessesLeft = u.guessesLeft - 1))
            Some("too low")
          }
          case d if (d < 0) => {
            this.addUser(u.copy(guessesLeft = u.guessesLeft - 1))
            Some("too high")
          }
        }
      } yield result

      result
    }
  }

  val db = Db()

  private val guessRoute = HttpRoutes.of[IO] {
    case req @ POST -> Root / "guess" => {
      val clientId = (for {
        cookie <- req.cookies.find(_.name == "id")
        id <- Some(cookie.content)
      } yield id).getOrElse(java.util.UUID.randomUUID.toString)

      def toLong(s: String): Option[Long] = {
        try {
          Some(s.toLong)
        } catch {
          case e: NumberFormatException => None
        }
      }

      val guess = for {
        g <- req.as[String]
      } yield toLong(g)

      val result = for {
        g <- guess
      } yield for {
        _ <- g
        if (g.isDefined)
        value <- g
        re <- db.guess(clientId, value)
      } yield re

      result.flatMap(r =>
        r match {
          case None    => BadRequest()
          case Some(n) => Ok(n)
        }
      )
    }
  }

  private val routes = {
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    case class Params(min: Long, max: Long, guesses: Long)

    HttpRoutes.of[IO] {

      case req @ POST -> Root / "start" => {
        req.as[Params].flatMap { params =>
          {
            val id = java.util.UUID.randomUUID.toString
            val number = Random.between(params.min, params.max + 1)
            db.addUser(User(id, number, params.guesses))
            Ok(
              s"You have ${params.guesses} attempts to guess a number between ${params.min} and ${params.max} (including)."
            ).map(_.addCookie("id", id))
          }
        }
      }

      case GET -> Root => {
        Ok("Guessing game.")
      }
    }
  }

  private[http] val httpApp = { guessRoute <+> routes }.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}

object GuessClient extends IOApp {
  import sttp.client3._

  import io.circe.generic.auto._
  import io.circe.syntax._

  final case class Params(min: Long, max: Long, guesses: Long)

  private val root = "http://localhost:9001"
  private val backend = HttpURLConnectionBackend()

  type SttpResponse = Identity[Response[Either[String, String]]]

  val min = 1L
  val max = 50L
  val guesses = 5L

  def guess(n: Long, r: SttpResponse) = {
    val res = basicRequest.post(uri"${root}/guess").body(n.toString).cookies(r).send(backend)
    println(res.body)
    res
  }

  def run(args: List[String]): IO[ExitCode] = {
    val params = Params(min, max, guesses)
    val paramsJson: Json = params.asJson

    val res = basicRequest.post(uri"${root}/start").body(paramsJson.toString()).send(backend)

    val guessed = ListBuffer[Long]()

    def getRandomNumber(min: Long, max: Long): Long = {
      val num = Random.between(min, max + 1)
      if (guessed.contains(num)) getRandomNumber(min, max)
      else {
        guessed.append(num)
        num
      }
    }

    var guessAgain = true

    while (guessAgain) {
      val num = getRandomNumber(min, max)
      println(s"Guessing $num")
      guessAgain = guess(num, res).body match {
        case Right(value) =>
          value match {
            case v if (v.take(2) == "No") => false
            case "correct"                => false
            case _                        => true
          }
        case Left(_) => false
      }
    }

    IO(ExitCode.Success)
  }
}
