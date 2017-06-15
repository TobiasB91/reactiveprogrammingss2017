import java.util.concurrent.TimeoutException

import scala.util.Random
import org.scalajs.dom._

import scala.concurrent.{Future, Promise}
import scala.scalajs.js.JavaScriptException
import scala.concurrent.ExecutionContext.Implicits.global

object Util {
  def choose[A](firstOption: A, otherOptions: A*): A =
    (firstOption +: otherOptions).apply(Random.nextInt(otherOptions.size + 1))

  def chooseFrom[A](range: Seq[A]): A = range.apply(Random.nextInt(range.size))

  def document: Future[html.Document] = {
    val doc = org.scalajs.dom.document
    doc.readyState match {
      case "loading" =>
        val p = Promise[html.Document]
        doc.addEventListener("DOMContentLoaded", (e: Event) => p.success(doc))
        p.future
      case "interactive" | "complete" =>
        Future.successful(doc)
    }
  }
}
