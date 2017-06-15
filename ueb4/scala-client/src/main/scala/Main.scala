import org.scalajs.dom._
import org.scalajs.dom.ext.KeyCode
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.JSApp
import scala.util.Random
import scala.concurrent.duration._
import pixijs._

/**
  * The entry point for the JS Space Shooter application
  *
  * @author Martin Ring <martin.ring@dfki.de>
  */
object Main extends JSApp {
  def main(): Unit = Util.document.foreach { document =>
    val backgrounds = Resources.loadBackgrounds()
    val spriteSheet = Resources.loadSpaceTextures()
    // Load space objects (depends on sprite sheet)
    val factory = spriteSheet.map(new SpaceObjectFactory(_))

    val socket = MessageSocket.open()

    // Create the pixi renderer
    val renderer = PIXI.autoDetectRenderer(0,0)

    renderer.autoResize = true
    renderer.resize(window.innerWidth.toInt,window.innerHeight.toInt)
    document.body.appendChild(renderer.view)

    // Start the render loop when resources are loaded
    val renderLoop = for {
      backgrounds <- backgrounds
      factory <- factory
      socket <- socket
    } yield {
      socket.send(Ping)
      val view = new SpaceView(factory, renderer, backgrounds)
      socket.onClose {
        view.text = "Connection closed!"
      }
      socket.onMessage { msg =>
        console.log(msg.toString)
        msg match {
          case TimestampedMessage (t, Asteroid (state, size, color)) =>
            var m = view.lookup(state.ident)
            m match {
              case Some (v) =>
                v.pos = Vector2d(state.pos._1, state.pos._2)
                v.orientation = state.omega
                v.angularVelocity = state.phi
                v.velocity = Vector2d(state.velo._1, state.velo._2)
                val delta = (System.currentTimeMillis() - t) / 1000
                v.update(delta)
              case None => 
                val s = size match {
                  case Tiny => Size.Tiny
                  case Small => Size.Small
                  case Medium => Size.Medium
                  case Big => Size.Big
                }
                val c = color match {
                  case Brown => Color.Brown
                  case Gray => Color.Grey
                }
                val meteor = view.factory.meteor(state.ident,s,c)
                meteor.pos = Vector2d(state.pos._1, state.pos._2) 
                meteor.orientation = state.omega 
                meteor.angularVelocity = state.phi
                meteor.velocity = Vector2d(state.velo._1, state.velo._2)
                view.spawn(meteor)
                val delta = (window.performance.now() - t) / 1000
                meteor.update(delta)
            }
          case _ => Unit
        }
      }
      populateExample(view)
      def loop(t0: Double)(now: Double): Unit = {
        val delta = (now - t0) / 1000
        view.update(delta)
        view.render()
        window.requestAnimationFrame(x => loop(now)(x))
      }
      val now = window.performance.now()
      loop(now)(now)
    }

    // Show error and print to console in case initialization failed
    renderLoop.failed.foreach(e => {
      window.alert(e.getMessage)
      throw e
    })
  }

  /* Example setup */
  def populateExample(view: SpaceView): Unit = {
    view.text = "Uebung 4: Curried in Space"
    val player = view.factory.player(Color.Blue)
   /* val meteors = Seq.fill(10) {
      val color = Util.choose(Color.Brown,Color.Grey)
      val size = Util.choose(Size.Big,Size.Medium,Size.Small,Size.Tiny)
      val meteor = view.factory.meteor(size,color)
      meteor.pos = Vector2d(500 - Random.nextDouble() * 1000, 500 - Random.nextDouble() * 1000)
      meteor.orientation = Random.nextDouble() * Math.PI * 2
      meteor.angularVelocity = Random.nextDouble() - 0.5
      meteor.velocity = Vector2d(50 - Random.nextDouble() * 100, 50 - Random.nextDouble() * 100)
      meteor
    }
    meteors.foreach(view.spawn)*/
    view.focus(Vector2d(0,0))
    val opts = new TextStyleOptions {
      fontFamily = "retro"
      fontSize = 16
      fill = "#ffffff"
      stroke = "#000000"
      strokeThickness = 4.0
    }
    val text = new pixijs.Text("Press SPACE to Start", opts)
    text.anchor.set(0.5,0.5)
    view.objectStage.addChild(text)
    view.setLifes(3)
    Control.waitFor(KeyCode.Space).foreach { _ =>
      view.addAmination(Animation.combine(
        Animation.fade(text, 1.0, 0.0, 0.5 seconds),
        Animation.scale(text, 1.0, 5.0, 0.5 seconds)
      )).done.foreach(_ => { view.objectStage.removeChild(text) })
      view.addAmination(Animation.fade(player.sprite, 0.0, 1.0, 0.5 seconds))
      view.spawn(player)
      view.focus(player)
      Control.bindKeyboard(player,view)
    }
  }
}
