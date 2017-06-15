import java.util.concurrent.TimeUnit

import pixijs.DisplayObject

import scala.concurrent.Future
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * @author Martin Ring <martin.ring@dfki.de>
  *
  * An animation is just a side-effecting function which takes time
  * deltas in seconds.
  */
sealed trait Animation {
  def update(delta: Double): Boolean
}

object Animation {
  /**
    * Create a new Animation from an animation function
    */
  def apply(f: Double => Boolean): Animation = new Animation {
    override def update(delta: Double): Boolean = f(delta)
  }

  /**
    * Comombines multiple animations in one
    */
  def combine(animations: Animation*): Animation = {
    var as = animations
    Animation { delta =>
      as = as.filter(_.update(delta))
      as.nonEmpty
    }
  }

  /**
    * Linear animation from `from` to `to` over the timespan declared in `duration`.
    * The values are passed to the `setter`
    */
  def linear(setter: Double => Unit, from: Double, to: Double, duration: FiniteDuration) = {
    val d = duration.toUnit(TimeUnit.SECONDS)
    var elapsed: Double = 0d
    Animation { delta =>
      elapsed = Math.min(elapsed + delta, d)
      setter(from + (to - from) * elapsed / d)
      elapsed != d
    }
  }

  /**
    * Fades the alpha value of a display object
    */
  def fade(obj: DisplayObject, from: Double, to: Double, duration: FiniteDuration) =
    linear(obj.alpha = _, from, to, duration)

  /**
    * Animates the scale of a display object
    */
  def scale(obj: DisplayObject, from: Double, to: Double, duration: FiniteDuration) =
    linear(x => obj.scale.set(x), from, to, duration)
}


/**
  * Represents a running animation
  */
trait RunningAnimation {
  /**
    * Future that completes, when the animation is over
    */
  def done: Future[Unit]

  /**
    * Cancel the execution of the animation. Will fail the future `done`
    */
  def cancel(): Unit
}