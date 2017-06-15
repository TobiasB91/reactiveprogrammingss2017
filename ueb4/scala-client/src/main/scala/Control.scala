import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw.{Event, KeyboardEvent}
import org.scalajs.dom.{html, window}

import scala.concurrent.{Future, Promise}
import scala.scalajs.js

/**
  * @author Martin Ring <martin.ring@dfki.de>
  */
object Control {
  /**
    * Wait for a key to be pressed. Returns a future, which completes when the key is pressed
    * for the first time.
    */
  def waitFor(keyCode: Int): Future[Unit] = {
    val promise = Promise[Unit]
    lazy val handler: js.Function1[Event,Unit] = e => if (e.asInstanceOf[KeyboardEvent].keyCode == keyCode) {
      promise.success(())
      window.removeEventListener("keydown",handler)
    }
    window.addEventListener("keydown",handler)
    promise.future
  }

  /**
    * Binds the keyboard control to a player ship
    */
  def bindKeyboard(playerShip: PlayerShip, view: SpaceView) = {
    window.addEventListener("keydown", (e: KeyboardEvent) => {
      e.keyCode match {
        case KeyCode.Left  => playerShip.angularVelocity = -3.0
        case KeyCode.Right => playerShip.angularVelocity = 3.0
        case KeyCode.Up => playerShip.acceleration = 500.0
        case KeyCode.Down => playerShip.acceleration = -200.0
        case KeyCode.Space =>
          Sound.shoot()
          //view.spawnBelow(view.factory.laser(playerShip), playerShip)
        case KeyCode.D =>
          playerShip.hit()
          playerShip.damageLevel += 1
        case _ =>
      }
    })

    window.addEventListener("keyup", (e: KeyboardEvent) => {
      e.keyCode match  {
        case KeyCode.Left  => playerShip.angularVelocity = 0.0
        case KeyCode.Right => playerShip.angularVelocity = 0.0
        case KeyCode.Up => playerShip.acceleration = 0.0
        case KeyCode.Down => playerShip.acceleration = 0.0
        case _ =>
      }
    })
  }
}
