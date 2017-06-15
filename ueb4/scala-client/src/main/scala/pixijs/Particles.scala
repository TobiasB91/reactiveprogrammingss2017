package pixijs.particles

import scala.scalajs.js
import js.annotation._
import js.|
import pixijs._

@js.native
trait ParticleConstructor extends js.Object {
  /* ??? ConstructorMember(FunSignature(List(),List(FunParam(Ident(emitter),false,Some(TypeRef(TypeName(Emitter),List())))),Some(TypeRef(TypeName(Particle),List())))) */
}

@js.native
trait AnimatedParticleArt extends js.Object {
  var textures: js.Array[String | Texture | js.Any] = js.native
  var framerate: Double | js.Any = js.native
  var loop: Boolean = js.native
}

@js.native
trait ParsedAnimatedParticleArt extends js.Object {
  var textures: js.Array[Texture] = js.native
  var framerate: Double = js.native
  var elapsed: Double = js.native
  var loop: Boolean = js.native
}

@js.native
@JSGlobal("PIXI.particles.AnimatedParticle")
class AnimatedParticle extends Particle {
  def applyArt(art: ParsedAnimatedParticleArt): Unit = js.native
}

@js.native
@JSGlobal("PIXI.particles.AnimatedParticle")
object AnimatedParticle extends js.Object {
  def parseArt(art: js.Array[AnimatedParticleArt]): js.Array[ParsedAnimatedParticleArt] = js.native
}

@js.native
@JSGlobal("PIXI.particles.Emitter")
class Emitter protected() extends js.Object {
  def this(particleParent: Container, particleImages: js.Any, config: js.Any) = this()

  var particleImages: js.Array[js.Any] = js.native
  var startAlpha: Double = js.native
  var endAlpha: Double = js.native
  var startSpeed: Double = js.native
  var endSpeed: Double = js.native
  var minimumSpeedMultiplier: Double = js.native
  var acceleration: Point = js.native
  var maxSpeed: Double = js.native
  var startScale: Double = js.native
  var endScale: Double = js.native
  var minimumScaleMultiplier: Double = js.native
  var startColor: String|js.Tuple3[Double, Double, Double] = js.native
  var endColor: String|js.Tuple3[Double, Double, Double] = js.native
  var minLifetime: Double = js.native
  var maxLifetime: Double = js.native
  var minStartRotation: Double = js.native
  var maxStartRotation: Double = js.native
  var noRotation: Boolean = js.native
  var minRotationSpeed: Double = js.native
  var maxRotationSpeed: Double = js.native
  var particleBlendMode: Double = js.native
  var customEase: js.Function1[Double, Double] = js.native
  var extraData: js.Any = js.native
  var maxParticles: Double = js.native
  var emitterLifetime: Double = js.native
  var spawnRect: Rectangle = js.native
  var spawnCircle: Circle = js.native
  var particlesPerWave: Double = js.native
  var particleSpacing: Double = js.native
  var angleStart: Double = js.native
  var addAtBack: Boolean = js.native
  var frequency: Double = js.native
  var particleConstructor: ParticleConstructor = js.native
  var parent: Container = js.native
  var emit: Boolean = js.native
  var autoUpdate: Boolean = js.native

  def init(art: js.Any, config: js.Any): Unit = js.native

  def rotate(newRot: Double): Unit = js.native

  def updateSpawnPos(x: Double, y: Double): Unit = js.native

  def updateOwnerPos(x: Double, y: Double): Unit = js.native

  def resetPositionTracking(): Unit = js.native

  def update(delta: Double): Unit = js.native

  def cleanup(): Unit = js.native

  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.particles.Particle")
class Particle protected() extends Sprite {
  def this(emitter: Emitter) = this()

  var emitter: Emitter = js.native
  var velocity: Point = js.native
  var maxLife: Double = js.native
  var age: Double = js.native
  var ease: js.Function1[Double, Double] = js.native
  var extraData: js.Any = js.native
  var startAlpha: Double = js.native
  var endAlpha: Double = js.native
  var startSpeed: Double = js.native
  var endSpeed: Double = js.native
  var acceleration: Point = js.native
  var maxSpeed: Double = js.native
  var startScale: Double = js.native
  var endScale: Double = js.native
  var startColor: js.Array[Double] = js.native
  var endColor: js.Array[Double] = js.native
  def init(): Unit = js.native
  def applyArt(art: js.Any): Unit = js.native
  def update(delta: Double): Double = js.native
  def kill(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.particles.Particle")
object Particle extends js.Object {
  def parseArt(art: js.Any): js.Dynamic = js.native

  def parseData(data: js.Any): js.Dynamic = js.native
}

@js.native
trait EaseSegment extends js.Object {
  var cp: Double = js.native
  var s: Double = js.native
  var e: Double = js.native
}

@js.native
@JSGlobal("PIXI.particles.ParticleUtils")
class ParticleUtils extends js.Object {
}

@js.native
@JSGlobal("PIXI.particles.ParticleUtils")
object ParticleUtils extends js.Object {
  var verbose: Boolean = js.native

  def rotatePoint(angle: Double, p: Point): Unit = js.native

  def combineRGBComponents(r: Double, g: Double, b: Double): Double = js.native

  def normalize(p: Point): Unit = js.native

  def scaleBy(p: Point, value: Double): Unit = js.native

  def length(p: Point): Double = js.native

  def hexToRGB(color: String, output: js.Tuple3[Double, Double, Double] = ???): js.Tuple3[Double, Double, Double] = js.native

  def generateEase(segments: js.Array[EaseSegment]): js.Function1[Double, Double] = js.native

  def getBlendMode(name: String): Double = js.native
}

@js.native
@JSGlobal("PIXI.particles.PathParticle")
class PathParticle extends Particle {
  var path: js.Function1[Double, Double] = js.native
  var initialRotation: Double = js.native
  var initialPosition: Point = js.native
  var movement: Double = js.native
}

@js.native
@JSGlobal("PIXI.particles.PathParticle")
object PathParticle extends js.Object {
  def parseArt(art: js.Array[String | Texture]): js.Array[Texture] = js.native
  def parseData(data: js.Any): js.Dynamic = js.native
}