import java.util.concurrent.CancellationException

import org.scalajs.dom.{Event, window}
import pixijs.extras.TilingSprite
import pixijs.{Container, Sprite, SystemRenderer, TextStyleOptions}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{CanAwait, ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.util.Try
import org.scalajs.dom._



class SpaceView(
  val factory: SpaceObjectFactory,
  renderer: SystemRenderer,
  backgrounds: Iterable[TilingSprite]) {

  private var animations_ = Map.empty[Animation,Promise[Unit]]
  private var objects_ = mutable.Set.empty[SpaceObject]
  private var focus_ : Either[Vector2d,SpaceObject] = Left(Vector2d(0,0))
  private val stage = new Container()
  private var size: Vector2d =
    Vector2d(renderer.view.width,renderer.view.height)

  val overlayStage = new Container()
  val objectStage = new Container()

  {
    backgrounds.foreach(stage.addChild(_))
    backgrounds.foreach(bg => {
      bg.width = window.innerWidth
      bg.height = window.innerHeight
    })
    window.addEventListener("resize", (e: Event) => {
      val newSize = Vector2d(window.innerWidth.toInt,window.innerHeight.toInt)
      val diff = newSize - size
      lifes.foreach(sprite => sprite.position.x += diff.x)
      size = newSize
      renderer.resize(size.x,size.y)
      backgrounds.foreach(bg => {
        bg.width = size.x
        bg.height = size.y
      })
    })
    stage.addChild(objectStage)
    stage.addChild(overlayStage)
  }

  private val textObj = {
    val opts = new TextStyleOptions {
      fontFamily = "retro"
      fontSize = 16
      fill = "#ffffff"
      stroke = "#000000"
      strokeThickness = 4.0
    }
    val text = new pixijs.Text("", opts)
    text.position.set(10, 10)
    overlayStage.addChild(text)
    text
  }

  def lookup(id : Int): Option[SpaceObject] = {    
      objects_.find(x => x.ident == id)
  }

  def text: String = textObj.text
  def text_=(value: String): Unit = textObj.text = value

  def focus(obj: SpaceObject): Unit = focus_ = Right(obj)
  def focus(vec: Vector2d): Unit = focus_ = Left(vec)

  def spawn(obj: SpaceObject): Unit = {
    objects_ += obj
    objectStage.addChild(obj.sprite)
  }

  def spawnBelow(obj: SpaceObject, other: SpaceObject): Unit = {
    objects_ += obj
    objectStage.addChildAt(obj.sprite,objectStage.getChildIndex(other.sprite))
  }

  def remove(obj: SpaceObject): Unit = {
    objects_ -= obj
    objectStage.removeChild(obj.sprite)
  }
  def objects: Traversable[SpaceObject] = objects_

  def addAmination(animation: Animation): RunningAnimation = {
    val p = Promise[Unit]
    animations_ += animation -> p
    new RunningAnimation {
      override def cancel(): Unit = {
        animations_ -= animation
        p.failure(new CancellationException("animation cancelled"))
      }
      override def done: Future[Unit] = p.future
    }
  }

  def focusPosition: Vector2d = focus_.fold(identity,_.pos)

  private var lifes = Iterable.empty[Sprite]
  def setLifes(n: Int, color: Color.Player = Color.Player.Blue, variant: Int = 0): Unit = {
    lifes.foreach(overlayStage.removeChild(_))
    val tex = factory.textures.players.lifes(color)(variant)
    val sprites = for (i <- 1 to n) yield {
      val sprite = new Sprite(tex)
      sprite.anchor.set(1,0)
      sprite.position.set(size.x - (10 + ((i-1) * (sprite.width + 5))),10)
      sprite
    }
    sprites.foreach(overlayStage.addChild(_))
    lifes = sprites
  }

  def update(delta: Double) = {
    animations_ = animations_.filter {
      case (a,p) if (a.update(delta)) => true
      case (a,p) =>
        p.success(())
        false
    }
    objects.foreach { obj =>
      obj.update(delta)
      if (!obj.exists) remove(obj)
    }
  }

  def render() = {
    val pos = focusPosition
    objectStage.position.set(- pos.x + size.x / 2, - pos.y + size.y / 2)
    backgrounds.zipWithIndex.foreach { case (level, i) =>
      val z = (backgrounds.size - i)
      val lx = level.width - (pos.x / (1.1 + z * 0.2))
      val ly = level.height - (pos.y / (1.1 + z * 0.2))
      level.asInstanceOf[js.Dynamic].tilePosition.x = lx
      level.asInstanceOf[js.Dynamic].tilePosition.y = ly
    }
    renderer.render(stage)
  }
}
