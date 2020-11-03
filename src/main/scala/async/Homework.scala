package async

import java.net.URL
import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.{util => ju}
import scala.collection.mutable
import scala.concurrent.Await
import scala.util.control.NonFatal

/**
  * Application:
  * - takes a web-page URL from arguments (args array)
  * - loads the web-page body, extracts HTTP links from it
  * - for all the found links, tries to fetch a server name header if there is one
  * - prints all the encountered unique server name values in alphabetical order
  *
  * Each link processing should be done in parallel.
  * Validation of arguments is not needed.
  *
  * Try to test it on http://google.com!
  */
object AsyncHomework extends App {
  if (args.length == 0) {
    println("No URL provided")
    System.exit(0)
  }

  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  (for {
    body <- fetchPageBody(args(0))
    links <- findLinkUrls(body)
  } yield links).onComplete(links =>
    links match {
      case Failure(exception) => println(exception)
      case Success(links)     => fetchLinks(links)
    }
  )

  def fetchLinks(links: List[String]) = {
    Future
      .sequence(links.map(link => fetchServerName(link)))
      .onComplete(servers =>
        servers match {
          case Success(servers)   => printSorted(servers.map(_.getOrElse("")))
          case Failure(exception) => println(exception)
        }
      )
  }

  def printSorted(l: List[String]) = {
    println("\n=== SERVERS ===\n")
    l.distinct.sorted(String.CASE_INSENSITIVE_ORDER.compare).foreach(println)
    println("\n=== END ===")
    System.exit(0)
  }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)("ISO-8859-1")
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }

  private def findLinkUrls(html: String): Future[List[String]] =
    Future {
      val linkPattern = """href="(http[^"]+)"""".r
      linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
    }
}
