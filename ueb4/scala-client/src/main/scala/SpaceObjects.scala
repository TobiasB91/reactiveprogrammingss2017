import Resources.SpaceTextures
import pixijs._
import pixijs.particles.Emitter
import collection.immutable.Iterable
import scala.scalajs.js

trait SpaceObject {
  val sprite: Sprite
 
  var ident : Int = -1

  def pos = Vector2d(sprite.position.x,sprite.position.y)
  def pos_=(value: Vector2d) = sprite.position.set(value.x,value.y)

  def orientation = sprite.rotation - Math.PI
  def orientation_=(value: Double) = sprite.rotation = (value + Math.PI)

  var angularVelocity = 0.0        // rad / s
  var velocity = new Vector2d(0,0) // px / s

  var acceleration = 0.0           // px / s ^ 2

  var exists: Boolean = true

  def alpha = sprite.alpha
  def alpha_=(value: Double) = sprite.alpha = value


  def remove() = {
    exists = false
  }

  def update(delta: Double) = { // delta: s
    velocity += Vector2d.unit.rotate(orientation) * (delta * acceleration)
    orientation += delta * angularVelocity
    pos += velocity * delta
  }
}

class SimpleSpaceObject(val id: Int, val sprite: Sprite, initialPos: Vector2d = Vector2d.zero, initialOrientation: Double = 0.0) extends SpaceObject {
  ident = id
  pos = initialPos
  orientation = initialOrientation

  def canEqual(other : Any) = other.isInstanceOf[SimpleSpaceObject]
  override def equals(other : Any) : Boolean = 
    other match {
      case other : SimpleSpaceObject => other.canEqual(this) && this.hashCode == other.hashCode
      case _ => false
    }
  override def hashCode : Int = {
    return ident
  }
}

/**
  * Creates Space Objects
  */
class SpaceObjectFactory(val textures: Resources.SpaceTextures) {
  def player(id : Int, color: Color.Player, variant: Int = 0): PlayerShip =
    new PlayerShip(id,color,variant)(textures)

  def meteor(id : Int, size: Size.Meteor, color: Color.Meteor): SpaceObject =
    new SimpleSpaceObject(id, new Sprite(Util.chooseFrom(textures.meteors(color)(size)))) {
      sprite.anchor.set(0.5,0.5)
      ident = id
    }

  def laser(id : Int, rel: SpaceObject, color: Color.Laser = Color.Laser.Red, variant: Int = 0) = {
    val sprite = new Sprite(textures.lasers(color)(variant))
    sprite.anchor.x = 0.5
    sprite.anchor.y = 0.0
    val laser = new SimpleSpaceObject(id, sprite, rel.pos, rel.orientation) {
      override def update(delta: Double) = {
        alpha -= delta / 2
        //if (alpha <= 0.0) this.remove()
        super.update(delta)
      }
    }
    laser.velocity = rel.velocity + Vector2d.unit.rotate(rel.orientation) * 1000
    laser
  }
}

class PlayerShip(id : Int, color: Color.Player, variant: Int)(textures: SpaceTextures) extends SpaceObject {
  val sprite = new Sprite(textures.players.ships(color)(variant))
  ident = id

  sprite.anchor.set(0.5,0.5)

  val thrustContainer = new Container()
  sprite.addChild(thrustContainer)

  val (thrustBL,thrustBR,thrustFL,thrustFR) = {
    import js.Dynamic.literal
    def thrustEmitterSettings(back: Boolean,left: Boolean) = literal (
      "alpha" -> literal ( "start" -> 0.8, "end" -> 0.0 ),
      "scale" -> literal ( "start" -> 0.1,  "end" -> 2 ),
      "color" -> literal ( "start" -> "ffffff",  "end" -> "331100" ),
      "speed" -> literal ( "start" -> (if (back) 300 else 250),  "end" -> (if (back) 300 else 250) ),
      "startRotation" -> (if (back) literal ( "min" -> 89, "max" -> 91 ) else literal ( "min" -> 265, "max" -> 275)),
      "lifetime" -> literal ( "min" -> 0.5, "max" -> 1 ),
      "frequency" -> (if (back) 0.003 else 0.003),
      "maxParticles" -> 256,
      "pos" -> literal ( "x" -> -45, "y" -> 25 ),
      "spawnType" -> "circle",
      "spawnCircle" -> new Circle(0,0,if(back)10 else 5)
    )

    def thrustEmitter(back: Boolean, left: Boolean) =
      new Emitter(thrustContainer,js.Array(textures.stars(2)),thrustEmitterSettings(back,left))

    (
      thrustEmitter(true,true),
      thrustEmitter(true,false),
      thrustEmitter(false,true),
      thrustEmitter(false,false)
    )
  }

  def updateThrust(delta: Double) = {
    thrustContainer.rotation = - sprite.rotation
    thrustBL.emit = acceleration > 0 || angularVelocity > 0 && acceleration == 0
    thrustBR.emit = acceleration > 0 || angularVelocity < 0 && acceleration == 0
    thrustFL.emit = acceleration < 0 || angularVelocity < 0 && acceleration == 0
    thrustFR.emit = acceleration < 0 || angularVelocity > 0 && acceleration == 0
    val rot = sprite.rotation * PIXI.RAD_TO_DEG
    thrustBL.rotate(rot)
    thrustBR.rotate(rot)
    thrustFL.rotate(rot)
    thrustFR.rotate(rot)
    val spawnBL = Vector2d(-45,25).rotate(sprite.rotation)
    val spawnBR = Vector2d(45,25).rotate(sprite.rotation)
    val spawnFL = Vector2d(-47,-8).rotate(sprite.rotation)
    val spawnFR = Vector2d(47,-8).rotate(sprite.rotation)
    thrustBL.updateSpawnPos(spawnBL.x,spawnBL.y)
    thrustBR.updateSpawnPos(spawnBR.x,spawnBR.y)
    thrustFL.updateSpawnPos(spawnFL.x,spawnFL.y)
    thrustFR.updateSpawnPos(spawnFR.x,spawnFR.y)
    thrustBL.update(delta)
    thrustBR.update(delta)
    thrustFL.update(delta)
    thrustFR.update(delta)
  }


  private val damages = textures.players.damage(variant).map { t =>
    val ds = new Sprite(t)
    ds.blendMode = PIXI.BLEND_MODES.MULTIPLY
    ds.anchor.set(0.5,0.5)
    ds
  }

  private var damageLevel_ = 0
  def damageLevel = damageLevel_
  def damageLevel_=(level: Int) = {
    damages.lift(damageLevel_ - 1).foreach(sprite.removeChild(_))
    damages.lift(level - 1).foreach(sprite.addChild(_))
    if (level > damages.size) damageLevel_ = 0 else damageLevel_ = level
  }

  private var damaged = 0.0
  private var damageFilter = new filters.TwistFilter()
  damageFilter.radius = sprite.width / 2
  damageFilter.offset = new Point(sprite.width / 2, sprite.height / 2)

  def hit() = damaged = 1.0
  sprite.filters = js.Array(damageFilter)

  override def update(delta: Double) = {
    super.update(delta)
    if (damaged > 0) {
      damaged = Math.max(0,damaged - delta)
      sprite.tint = 0xffffff - 0x000101 * (damaged * 255).toInt
      damageFilter.angle = damaged * 360
    } else if (damaged == 0) {
      damaged = -1
      sprite.tint = 0xffffff
      damageFilter.angle = 0
    }
    updateThrust(delta)
  }
}
