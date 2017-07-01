import akka.NotUsed
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.Source
import org.scalajs.dom.Event

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

/**
  * Created by martin on 6/15/17.
  */
object Util {
  implicit class EventSource(val target: org.scalajs.dom.EventTarget) {
    def on[E <: Event](event: String): Source[E,NotUsed] =
      Source.queue[E](2048, OverflowStrategy.fail).mapMaterializedValue { queue =>
        val handler: scalajs.js.Function1[E, Unit] = (e: E) => queue.offer(e)
        target.addEventListener(event, handler)
        handler
      }.watchTermination() {
        case (h, d) =>
          d.onComplete(_ => target.removeEventListener(event, h))
          NotUsed
      }

    def once[E <: Event](event: String): Future[E] = {
      val p = Promise[E]
      val handler: scalajs.js.Function1[E, Unit] = (e: E) => p.success(e)
      target.addEventListener(event, handler)
      p.future.map { e =>
        target.removeEventListener(event, handler)
        e
      }
    }
  }
}
