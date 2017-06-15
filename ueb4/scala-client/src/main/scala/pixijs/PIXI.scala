package pixijs

import scala.scalajs.js
import js.annotation._
import js.|
import scala.scalajs.js.typedarray.{ Float32Array, ArrayBuffer, ArrayBufferView, Uint16Array, Uint32Array }
import scala.scalajs.js.RegExp
import org.scalajs.dom.raw._

@JSGlobal("PIXI")
@js.native
object PIXI extends js.Object {
  val VERSION: String = js.native
  val PI_2: Double = js.native
  val RAD_TO_DEG: Double = js.native
  val DEG_TO_RAD: Double = js.native
  val RENDERER_TYPE: CONST.RENDERER_TYPE.type = js.native
  val BLEND_MODES: CONST.BLEND_MODES.type = js.native
  val DRAW_MODES: CONST.DRAW_MODES.type = js.native
  val SCALE_MODES: CONST.SCALE_MODES.type = js.native
  val WRAP_MODES: CONST.WRAP_MODES.type = js.native
  val TRANSFORM_MODE: CONST.TRANSFORM_MODE.type = js.native
  val PRECISION: CONST.PRECISION.type = js.native
  val GC_MODES: CONST.GC_MODES.type = js.native
  val SHAPES: CONST.SHAPES.type = js.native
  val TEXT_GRADIENT: CONST.TEXT_GRADIENT.type = js.native
  val UPDATE_PRIORITY: CONST.UPDATE_PRIORITY.type = js.native
  def autoDetectRenderer(width: Double, height: Double, options: RendererOptions = ???, forceCanvas: Boolean = ???): SystemRenderer = js.native
  def autoDetectRenderer(options: RendererOptions): SystemRenderer = js.native
  def autoDetectRenderer(): SystemRenderer = js.native
  val loader: loaders.Loader = js.native
}

@JSGlobal("PIXI.CONST")
@js.native
object CONST extends js.Object {
  val VERSION: String = js.native
  val PI_2: Double = js.native
  val RAD_TO_DEG: Double = js.native
  val DEG_TO_RAD: Double = js.native
  val TARGET_FPMS: Double = js.native
  val URL_FILE_EXTENSION: RegExp | String = js.native
  val DATA_URI: RegExp | String = js.native
  val SVG_SIZE: RegExp | String = js.native

  @js.native
  object RENDERER_TYPE extends js.Object {
    val UNKNOWN: Double = js.native
    val WEBGL: Double = js.native
    val CANVAS: Double = js.native
  }

  @js.native
  object BLEND_MODES extends js.Object {
    val NORMAL: Double = js.native
    val ADD: Double = js.native
    val MULTIPLY: Double = js.native
    val SCREEN: Double = js.native
    val OVERLAY: Double = js.native
    val DARKEN: Double = js.native
    val LIGHTEN: Double = js.native
    val COLOR_DODGE: Double = js.native
    val COLOR_BURN: Double = js.native
    val HARD_LIGHT: Double = js.native
    val SOFT_LIGHT: Double = js.native
    val DIFFERENCE: Double = js.native
    val EXCLUSION: Double = js.native
    val HUE: Double = js.native
    val SATURATION: Double = js.native
    val COLOR: Double = js.native
    val LUMINOSITY: Double = js.native
  }

  @js.native
  object DRAW_MODES extends js.Object {
    val POINTS: Double = js.native
    val LINES: Double = js.native
    val LINE_LOOP: Double = js.native
    val LINE_STRIP: Double = js.native
    val TRIANGLES: Double = js.native
    val TRIANGLE_STRIP: Double = js.native
    val TRIANGLE_FAN: Double = js.native
  }

  @js.native
  object SCALE_MODES extends js.Object {
    val LINEAR: Double = js.native
    val NEAREST: Double = js.native
  }

  @js.native
  object GC_MODES extends js.Object {
    val AUTO: Double = js.native
    val MANUAL: Double = js.native
  }

  @js.native
  object WRAP_MODES extends js.Object {
    val CLAMP: Double = js.native
    val MIRRORED_REPEAT: Double = js.native
    val REPEAT: Double = js.native
  }

  @js.native
  object TRANSFORM_MODE extends js.Object {
    val DEFAULT: Double = js.native
    val DYNAMIC: Double = js.native
    val STATIC: Double = js.native
  }

  @js.native
  object SHAPES extends js.Object {
    val POLY: Double = js.native
    val RECT: Double = js.native
    val CIRC: Double = js.native
    val ELIP: Double = js.native
    val RREC: Double = js.native
  }

  @js.native
  object PRECISION extends js.Object {
    val LOW: String = js.native
    val MEDIUM: String = js.native
    val HIGH: String = js.native
  }

  @js.native
  object TEXT_GRADIENT extends js.Object {
    val LINEAR_VERTICAL: Double = js.native
    val LINEAR_HORIZONTAL: Double = js.native
  }

  @js.native
  object UPDATE_PRIORITY extends js.Object {
    val INTERACTION: Double = js.native
    val HIGH: Double = js.native
    val NORMAL: Double = js.native
    val LOW: Double = js.native
    val UTILITY: Double = js.native
  }
}


  package settings {

@js.native
@JSGlobal("PIXI.settings.RENDER_OPTIONS")
object RENDER_OPTIONS extends js.Object {
  val view: HTMLCanvasElement | Null = js.native
  val antialias: Boolean = js.native
  val forceFXAA: Boolean = js.native
  val autoResize: Boolean = js.native
  val transparent: Boolean = js.native
  val backgroundColor: Double = js.native
  val clearBeforeRender: Boolean = js.native
  val preserveDrawingBuffer: Boolean = js.native
  val roundPixels: Boolean = js.native
  val width: Double = js.native
  val height: Double = js.native
  val legacy: Boolean = js.native
}

@JSGlobal("PIXI.settings")
@js.native
object Settings extends js.Object {
  var TARGET_FPMS: Double = js.native
  var MIPMAP_TEXTURES: Boolean = js.native
  var RESOLUTION: Double = js.native
  var FILTER_RESOLUTION: Double = js.native
  var SPRITE_MAX_TEXTURES: Double = js.native
  var SPRITE_BATCH_SIZE: Double = js.native
  var RETINA_PREFIX: RegExp = js.native
  var TRANSFORM_MODE: Double = js.native
  var GC_MODE: Double = js.native
  var GC_MAX_IDLE: Double = js.native
  var GC_MAX_CHECK_COUNT: Double = js.native
  var WRAP_MODE: Double = js.native
  var SCALE_MODE: Double = js.native
  var PRECISION_VERTEX: String = js.native
  var PRECISION_FRAGMENT: String = js.native
  var PRECISION: String = js.native
  var UPLOADS_PER_FRAME: Double = js.native
  var CAN_UPLOAD_SAME_BUFFER: Boolean = js.native
  type PRECISION = Double
}

}

package accessibility {
  @js.native
  @JSGlobal("PIXI.accessibility.AccessibilityManager")
  class AccessibilityManager protected() extends js.Object {
    def this(renderer: CanvasRenderer | WebGLRenderer) = this()

    var debug: Boolean = js.native
    var renderer: SystemRenderer = js.native

    def destroy(): Unit = js.native
  }

  @js.native
  trait AccessibleTarget extends js.Object {
    var accessible: Boolean = js.native
    var accessibleTitle: String | Null = js.native
    var accessibleHint: String | Null = js.native
    var tabIndex: Double = js.native
  }

}



@js.native
trait ApplicationOptions extends RendererOptions {
  var preserveDrawingBuffer: js.UndefOr[Boolean] = js.undefined
  var legacy: js.UndefOr[Boolean] = js.undefined
  var sharedTicker: js.UndefOr[Boolean] = js.undefined
  var sharedLoader: js.UndefOr[Boolean] = js.undefined
}

@js.native
@JSGlobal("PIXI.Application")
class Application() extends js.Object {
  def this(options: ApplicationOptions) = this()
  def this(width: Double = ???, height: Double = ???, options: ApplicationOptions = ???, noWebGL: Boolean = ???, sharedTicker: Boolean = ???, sharedLoader: Boolean = ???) = this()
  var renderer: WebGLRenderer | CanvasRenderer = js.native
  var stage: Container = js.native
  var ticker: pixijs.ticker.Ticker = js.native
  def stop(): Unit = js.native
  def start(): Unit = js.native
  def render(): Unit = js.native
  def destroy(removeView: Boolean = ???): Unit = js.native
}

@js.native
trait DestroyOptions extends js.Object {
  var children: Boolean = js.native
  var texture: Boolean = js.native
  var baseTexture: Boolean = js.native
}

@js.native
@JSGlobal("PIXI.Bounds")
class Bounds extends js.Object {
  var minX: Double = js.native
  var minY: Double = js.native
  var maxX: Double = js.native
  var maxY: Double = js.native
  var rect: Rectangle = js.native
  def isEmpty(): Boolean = js.native
  def clear(): Unit = js.native
  def getRectangle(rect: Rectangle = ???): Rectangle = js.native
  def addPoint(point: Point): Unit = js.native
  def addQuad(vertices: js.Array[Double]): Bounds | Unit = js.native
  def addFrame(transform: Transform, x0: Double, y0: Double, x1: Double, y1: Double): Unit = js.native
  def addVertices(transform: Transform, vertices: js.Array[Double], beginOffset: Double, endOffset: Double): Unit = js.native
  def addBounds(bounds: Bounds): Unit = js.native
  def addBoundsMask(bounds: Bounds, mask: Bounds): Unit = js.native
  def addBoundsArea(bounds: Bounds, area: Rectangle): Unit = js.native
}

@js.native
@JSGlobal("PIXI.Container")
class Container extends DisplayObject {
  def getChildByName(name: String): DisplayObject = js.native
  var children: js.Array[DisplayObject] = js.native
  var width: Double = js.native
  var height: Double = js.native
  def addChild[T <: DisplayObject](child: T, additionalChildren: DisplayObject*): T = js.native
  def addChildAt[T <: DisplayObject](child: T, index: Double): T = js.native
  def swapChildren(child: DisplayObject, child2: DisplayObject): Unit = js.native
  def getChildIndex(child: DisplayObject): Double = js.native
  def setChildIndex(child: DisplayObject, index: Double): Unit = js.native
  def getChildAt(index: Double): DisplayObject = js.native
  def removeChild(child: DisplayObject): DisplayObject = js.native
  def removeChildAt(index: Double): DisplayObject = js.native
  def removeChildren(beginIndex: Double = ???, endIndex: Double = ???): js.Array[DisplayObject] = js.native
  def calculateBounds(): Unit = js.native
  def renderAdvancedWebGL(renderer: WebGLRenderer): Unit = js.native
  def destroy(options: DestroyOptions | Boolean = ???): Unit = js.native
}

@js.native
@JSGlobal("PIXI.DisplayObject")
class DisplayObject extends utils.EventEmitter with interaction.InteractiveTarget with accessibility.AccessibleTarget {
  var cacheAsBitmap: Boolean = js.native
  var name: String | Null = js.native
  def getGlobalPosition(point: Point = ???, skipUpdate: Boolean = ???): Point = js.native
  var transform: TransformBase = js.native
  var alpha: Double = js.native
  var visible: Boolean = js.native
  var renderable: Boolean = js.native
  var parent: Container = js.native
  var worldAlpha: Double = js.native
  var filterArea: Rectangle = js.native
  var x: Double = js.native
  var y: Double = js.native
  var worldTransform: Matrix = js.native
  var localTransform: Matrix = js.native
  var position: ObservablePoint = js.native
  var scale: ObservablePoint = js.native
  var pivot: ObservablePoint = js.native
  var skew: ObservablePoint = js.native
  var rotation: Double = js.native
  var worldVisible: Boolean = js.native
  var mask: Graphics | Sprite = js.native
  var filters: js.Array[Filter] = js.native
  def updateTransform(): Unit = js.native
  def getBounds(skipUpdate: Boolean = ???, rect: Rectangle = ???): Rectangle = js.native
  def getLocalBounds(rect: Rectangle = ???): Rectangle = js.native
  def toGlobal(position: Point, point: Point = ???, skipUpdate: Boolean = ???): Point = js.native
  def toLocal(position: Point, from: DisplayObject = ???, point: Point = ???, skipUpdate: Boolean = ???): Point = js.native
  def renderWebGL(renderer: WebGLRenderer): Unit = js.native
  def renderCanvas(renderer: CanvasRenderer): Unit = js.native
  def setParent(container: Container): Container = js.native
  def setTransform(x: Double = ???, y: Double = ???, scaleX: Double = ???, scaleY: Double = ???, rotation: Double = ???, skewX: Double = ???, skewY: Double = ???, pivotX: Double = ???, pivotY: Double = ???): DisplayObject = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.TransformBase")
class TransformBase extends js.Object {
  var worldTransform: Matrix = js.native
  var localTransform: Matrix = js.native
  def updateLocalTransform(): Unit = js.native
  def updateTransform(parentTransform: TransformBase): Unit = js.native
  def updateWorldTransform(parentTransform: TransformBase): Unit = js.native
}

@js.native
@JSGlobal("PIXI.TransformBase")
object TransformBase extends js.Object {
  var IDENTITY: TransformBase = js.native
}

@js.native
@JSGlobal("PIXI.TransformStatic")
class TransformStatic extends TransformBase {
  var position: ObservablePoint = js.native
  var scale: ObservablePoint = js.native
  var pivot: ObservablePoint = js.native
  var skew: ObservablePoint = js.native
  def updateSkew(): Unit = js.native
  def setFromMatrix(matrix: Matrix): Unit = js.native
  var rotation: Double = js.native
}

@js.native
@JSGlobal("PIXI.Transform")
class Transform extends TransformBase {
  var position: Point = js.native
  var scale: Point = js.native
  var skew: ObservablePoint = js.native
  var pivot: Point = js.native
  def updateSkew(): Unit = js.native
  def setFromMatrix(matrix: Matrix): Unit = js.native
  var rotation: Double = js.native
}

@js.native
@JSGlobal("PIXI.GraphicsData")
class GraphicsData protected () extends js.Object {
  def this(lineWidth: Double, lineColor: Double, lineAlpha: Double, fillColor: Double, fillAlpha: Double, fill: Boolean, nativeLines: Boolean, shape: Circle | Rectangle | Ellipse | Polygon | RoundedRectangle | js.Any) = this()
  var lineWidth: Double = js.native
  var nativeLines: Boolean = js.native
  var lineColor: Double = js.native
  var lineAlpha: Double = js.native
  var fillColor: Double = js.native
  var fillAlpha: Double = js.native
  var fill: Boolean = js.native
  var shape: Circle | Rectangle | Ellipse | Polygon | RoundedRectangle | js.Any = js.native
  var `type`: Double = js.native
  @JSName("clone")
  def clone_(): GraphicsData = js.native
  def addHole(shape: Circle | Rectangle | Ellipse | Polygon | RoundedRectangle | js.Any): Unit = js.native
  def destroy(options: DestroyOptions | Boolean = ???): Unit = js.native
}

@js.native
@JSGlobal("PIXI.Graphics")
class Graphics protected () extends Container {
  def this(nativeLines: Boolean = ???) = this()
  var fillAlpha: Double = js.native
  var lineWidth: Double = js.native
  var nativeLines: Boolean = js.native
  var lineColor: Double = js.native
  var tint: Double = js.native
  var blendMode: Double = js.native
  var currentPath: GraphicsData = js.native
  var isMask: Boolean = js.native
  var boundsPadding: Double = js.native
  var dirty: Double = js.native
  var fastRectDirty: Double = js.native
  var clearDirty: Double = js.native
  var boundsDirty: Double = js.native
  @JSName("clone")
  def clone_(): Graphics = js.native
  def lineStyle(lineWidth: Double = ???, color: Double = ???, alpha: Double = ???): Graphics = js.native
  def moveTo(x: Double, y: Double): Graphics = js.native
  def lineTo(x: Double, y: Double): Graphics = js.native
  def quadraticCurveTo(cpX: Double, cpY: Double, toX: Double, toY: Double): Graphics = js.native
  def bezierCurveTo(cpX: Double, cpY: Double, cpX2: Double, cpY2: Double, toX: Double, toY: Double): Graphics = js.native
  def arcTo(x1: Double, y1: Double, x2: Double, y2: Double, radius: Double): Graphics = js.native
  def arc(cx: Double, cy: Double, radius: Double, startAngle: Double, endAngle: Double, anticlockwise: Boolean = ???): Graphics = js.native
  def beginFill(color: Double, alpha: Double = ???): Graphics = js.native
  def endFill(): Graphics = js.native
  def drawRect(x: Double, y: Double, width: Double, height: Double): Graphics = js.native
  def drawRoundedRect(x: Double, y: Double, width: Double, height: Double, radius: Double): Graphics = js.native
  def drawCircle(x: Double, y: Double, radius: Double): Graphics = js.native
  def drawEllipse(x: Double, y: Double, width: Double, height: Double): Graphics = js.native
  def drawPolygon(path: js.Array[Double] | js.Array[Point]): Graphics = js.native
  def clear(): Graphics = js.native
  def isFastRect(): Boolean = js.native
  def containsPoint(point: Point): Boolean = js.native
  def updateLocalBounds(): Unit = js.native
  def drawShape(shape: Circle | Rectangle | Ellipse | Polygon | RoundedRectangle | js.Any): GraphicsData = js.native
  def generateCanvasTexture(scaleMode: Double = ???, resolution: Double = ???): Texture = js.native
}

@js.native
@JSGlobal("PIXI.Graphics")
object Graphics extends js.Object {
  var _SPRITE_TEXTURE: Texture = js.native
}

@js.native
@JSGlobal("PIXI.CanvasGraphicsRenderer")
class CanvasGraphicsRenderer protected () extends js.Object {
  def this(renderer: SystemRenderer) = this()
  def render(graphics: Graphics): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.GraphicsRenderer")
class GraphicsRenderer protected () extends ObjectRenderer {
  def this(renderer: CanvasRenderer) = this()
  var gl: WebGLRenderingContext = js.native
  var CONTEXT_UID: Double = js.native
  def render(graphics: Graphics): Unit = js.native
  def getWebGLData(webGL: WebGLRenderingContext, `type`: Double, nativeLines: Double): WebGLGraphicsData = js.native
}

@js.native
@JSGlobal("PIXI.WebGLGraphicsData")
class WebGLGraphicsData protected () extends js.Object {
  def this(gl: WebGLRenderingContext, shader: glCore.GLShader, attribsState: glCore.AttribState) = this()
  var gl: WebGLRenderingContext = js.native
  var color: js.Array[Double] = js.native
  var points: js.Array[Point] = js.native
  var indices: js.Array[Double] = js.native
  var buffer: WebGLBuffer = js.native
  var indexBuffer: WebGLBuffer = js.native
  var dirty: Boolean = js.native
  var glPoints: js.Array[Double] = js.native
  var glIndices: js.Array[Double] = js.native
  var shader: glCore.GLShader = js.native
  var vao: glCore.VertexArrayObject = js.native
  var nativeLines: Boolean = js.native
  def reset(): Unit = js.native
  def upload(): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.PrimitiveShader")
class PrimitiveShader extends glCore.GLShader {
}

package GroupD8 {

@JSGlobal("PIXI.GroupD8")
@js.native
object GroupD8 extends js.Object {
  val E: Double = js.native
  val SE: Double = js.native
  val S: Double = js.native
  val SW: Double = js.native
  val W: Double = js.native
  val NW: Double = js.native
  val N: Double = js.native
  val NE: Double = js.native
  val MIRROR_HORIZONTAL: Double = js.native
  val MIRROR_VERTICAL: Double = js.native
  def uX(ind: Double): Double = js.native
  def uY(ind: Double): Double = js.native
  def vX(ind: Double): Double = js.native
  def vY(ind: Double): Double = js.native
  def inv(rotation: Double): Double = js.native
  def add(rotationSecond: Double, rotationFirst: Double): Double = js.native
  def sub(rotationSecond: Double, rotationFirst: Double): Double = js.native
  def rotate180(rotation: Double): Double = js.native
  def isSwapWidthHeight(rotation: Double): Boolean = js.native
  def byDirection(dx: Double, dy: Double): Double = js.native
  def matrixAppendRotationInv(matrix: Matrix, rotation: Double, tx: Double, ty: Double): Unit = js.native
}

}

@js.native
@JSGlobal("PIXI.Matrix")
class Matrix protected () extends js.Object {
  def this(a: Double = ???, b: Double = ???, c: Double = ???, d: Double = ???, tx: Double = ???, ty: Double = ???) = this()
  var a: Double = js.native
  var b: Double = js.native
  var c: Double = js.native
  var d: Double = js.native
  var tx: Double = js.native
  var ty: Double = js.native
  def fromArray(array: js.Array[Double]): Unit = js.native
  def set(a: Double, b: Double, c: Double, d: Double, tx: Double, ty: Double): Matrix = js.native
  def toArray(transpose: Boolean = ???, out: js.Array[Double] = ???): js.Array[Double] = js.native
  @JSName("apply")
  def apply(pos: Point, newPos: Point = ???): Point = js.native
  def applyInverse(pos: Point, newPos: Point = ???): Point = js.native
  def translate(x: Double, y: Double): Matrix = js.native
  def scale(x: Double, y: Double): Matrix = js.native
  def rotate(angle: Double): Matrix = js.native
  def append(matrix: Matrix): Matrix = js.native
  def setTransform(x: Double, y: Double, pivotX: Double, pivotY: Double, scaleX: Double, scaleY: Double, rotation: Double, skewX: Double, skewY: Double): Matrix = js.native
  def prepend(matrix: Matrix): Matrix = js.native
  def invert(): Matrix = js.native
  def identity(): Matrix = js.native
  def decompose(transform: TransformBase): TransformBase = js.native
  @JSName("clone")
  def clone_(): Matrix = js.native
  def copy(matrix: Matrix): Matrix = js.native
}

@js.native
@JSGlobal("PIXI.Matrix")
object Matrix extends js.Object {
  var IDENTITY: Matrix = js.native
  var TEMP_MATRIX: Matrix = js.native
}

@js.native
@JSGlobal("PIXI.ObservablePoint")
class ObservablePoint protected () extends js.Object {
  def this(cb: js.Function0[Any], scope: js.Any = ???, x: Double = ???, y: Double = ???) = this()
  var x: Double = js.native
  var y: Double = js.native
  var cb: js.Function0[Any] = js.native
  var scope: js.Any = js.native
  def set(x: Double = ???, y: Double = ???): Unit = js.native
  def copy(point: Point | ObservablePoint): Unit = js.native
}

@js.native
@JSGlobal("PIXI.Point")
class Point protected () extends js.Object {
  def this(x: Double = ???, y: Double = ???) = this()
  var x: Double = js.native
  var y: Double = js.native
  def copy(p: Point | ObservablePoint): Unit = js.native
  def equals(p: Point): Boolean = js.native
  def set(x: Double = ???, y: Double = ???): Unit = js.native
}

@js.native
trait HitArea extends js.Object {
  def contains(x: Double, y: Double): Boolean = js.native
}

@js.native
@JSGlobal("PIXI.Circle")
class Circle protected () extends js.Object {
  def this(x: Double = ???, y: Double = ???, radius: Double = ???) = this()
  var x: Double = js.native
  var y: Double = js.native
  var radius: Double = js.native
  var `type`: Double = js.native
  @JSName("clone")
  def clone_(): Circle = js.native
  def contains(x: Double, y: Double): Boolean = js.native
  def getBounds(): Rectangle = js.native
}

@js.native
@JSGlobal("PIXI.Ellipse")
class Ellipse protected () extends js.Object {
  def this(x: Double = ???, y: Double = ???, width: Double = ???, height: Double = ???) = this()
  var x: Double = js.native
  var y: Double = js.native
  var width: Double = js.native
  var height: Double = js.native
  var `type`: Double = js.native
  @JSName("clone")
  def clone_(): Ellipse = js.native
  def contains(x: Double, y: Double): Boolean = js.native
  def getBounds(): Rectangle = js.native
}

@js.native
@JSGlobal("PIXI.Polygon")
class Polygon protected () extends js.Object {
  def this(points: js.Array[Point] | js.Array[Double]) = this()
  def this(points: Point*) = this()
  var closed: Boolean = js.native
  var points: js.Array[Double] = js.native
  var `type`: Double = js.native
  @JSName("clone")
  def clone_(): Polygon = js.native
  def contains(x: Double, y: Double): Boolean = js.native
  def close(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.Rectangle")
class Rectangle protected () extends js.Object {
  def this(x: Double = ???, y: Double = ???, width: Double = ???, height: Double = ???) = this()
  var x: Double = js.native
  var y: Double = js.native
  var width: Double = js.native
  var height: Double = js.native
  var `type`: Double = js.native
  var left: Double = js.native
  var right: Double = js.native
  var top: Double = js.native
  var bottom: Double = js.native
  @JSName("clone")
  def clone_(): Rectangle = js.native
  def copy(rectangle: Rectangle): Rectangle = js.native
  def contains(x: Double, y: Double): Boolean = js.native
  def pad(paddingX: Double, paddingY: Double): Unit = js.native
  def fit(rectangle: Rectangle): Unit = js.native
  def enlarge(rectangle: Rectangle): Unit = js.native
}

@js.native
@JSGlobal("PIXI.Rectangle")
object Rectangle extends js.Object {
  var EMPTY: Rectangle = js.native
}

@js.native
@JSGlobal("PIXI.RoundedRectangle")
class RoundedRectangle protected () extends js.Object {
  def this(x: Double = ???, y: Double = ???, width: Double = ???, height: Double = ???, radius: Double = ???) = this()
  var x: Double = js.native
  var y: Double = js.native
  var width: Double = js.native
  var height: Double = js.native
  var radius: Double = js.native
  var `type`: Double = js.native
  @JSName("clone")
  def clone_(): RoundedRectangle = js.native
  def contains(x: Double, y: Double): Boolean = js.native
}

@ScalaJSDefined
trait RendererOptions extends js.Object {
  var view: js.UndefOr[HTMLCanvasElement] = js.undefined
  var transparent: js.UndefOr[Boolean] = js.undefined
  var autoResize: js.UndefOr[Boolean] = js.undefined
  var antialias: js.UndefOr[Boolean] = js.undefined
  var resolution: js.UndefOr[Double] = js.undefined
  var clearBeforeRender: js.UndefOr[Boolean] = js.undefined
  var backgroundColor: js.UndefOr[Double] = js.undefined
  var roundPixels: js.UndefOr[Boolean] = js.undefined
  var context: js.UndefOr[WebGLRenderingContext] = js.undefined
  var width: js.UndefOr[Double] = js.undefined
  var height: js.UndefOr[Double] = js.undefined
  var forceCanvas: js.UndefOr[Boolean] = js.undefined
}

@js.native
@JSGlobal("PIXI.SystemRenderer")
class SystemRenderer protected () extends utils.EventEmitter {
  def this(system: String) = this()
  def this(system: String, options: RendererOptions) = this()
  def this(system: String, screenWidth: Double = ???, screenHeight: Double = ???, options: RendererOptions = ???) = this()
  var `type`: Double = js.native
  var options: RendererOptions = js.native
  var screen: Rectangle = js.native
  var view: HTMLCanvasElement = js.native
  var resolution: Double = js.native
  var transparent: Boolean = js.native
  var autoResize: Boolean = js.native
  var blendModes: js.Any = js.native
  var preserveDrawingBuffer: Boolean = js.native
  var clearBeforeRender: Boolean = js.native
  var roundPixels: Boolean = js.native
  var backgroundColor: Double = js.native
  def resize(screenWidth: Double, screenHeight: Double): Unit = js.native
  def generateTexture(displayObject: DisplayObject, scaleMode: Double = ???, resolution: Double = ???): RenderTexture = js.native
  def render(args: js.Any*): Unit = js.native
  def destroy(removeView: Boolean = ???): Unit = js.native
}

@js.native
@JSGlobal("PIXI.CanvasRenderer")
class CanvasRenderer () extends SystemRenderer {
  def this(options: RendererOptions) = this()
  def this(screenWidth: Double = ???, screenHeight: Double = ???, options: RendererOptions = ???) = this()
  var plugins: js.Any = js.native
  def initPlugins(): Unit = js.native
  def destroyPlugins(): Unit = js.native
  var interaction: pixijs.interaction.InteractionManager = js.native
  var rootContext: CanvasRenderingContext2D = js.native
  var rootResolution: Double = js.native
  var refresh: Boolean = js.native
  var maskManager: CanvasMaskManager = js.native
  var smoothProperty: String = js.native
  var extract: pixijs.extract.CanvasExtract = js.native
  var context: CanvasRenderingContext2D | Null = js.native
  def render(displayObject: DisplayObject, renderTexture: RenderTexture = ???, clear: Boolean = ???, transform: Transform = ???, skipUpdateTransform: Boolean = ???): Unit = js.native
  def setBlendMode(blendMode: Double): Unit = js.native
  def clear(clearColor: String = ???): Unit = js.native
}

@js.native
@JSGlobal("PIXI.CanvasRenderer")
object CanvasRenderer extends js.Object {
  var __plugins: js.Any = js.native
  def registerPlugin(pluginName: String, ctor: js.Function): Unit = js.native
}

@js.native
@JSGlobal("PIXI.CanvasMaskManager")
class CanvasMaskManager protected () extends js.Object {
  def this(renderer: CanvasRenderer) = this()
  def pushMask(maskData: js.Any): Unit = js.native
  def popMask(renderer: WebGLRenderer | CanvasRenderer): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.CanvasRenderTarget")
class CanvasRenderTarget protected () extends js.Object {
  def this(width: Double, height: Double, resolution: Double) = this()
  var canvas: HTMLCanvasElement = js.native
  var context: CanvasRenderingContext2D = js.native
  var resolution: Double = js.native
  var width: Double = js.native
  var height: Double = js.native
  def clear(): Unit = js.native
  def resize(width: Double, height: Double): Unit = js.native
  def destroy(): Unit = js.native
}

@ScalaJSDefined
trait WebGLRendererOptions extends js.Object {
  var view: js.UndefOr[HTMLCanvasElement] = js.undefined
  var transparent: js.UndefOr[Boolean] = js.undefined
  var autoResize: js.UndefOr[Boolean] = js.undefined
  var antialias: js.UndefOr[Boolean] = js.undefined
  var forceFXAA: js.UndefOr[Boolean] = js.undefined
  var resolution: js.UndefOr[Double] = js.undefined
  var clearBeforeRender: js.UndefOr[Boolean] = js.undefined
  var backgroundColor: js.UndefOr[Double] = js.undefined
  var preserveDrawingBuffer: js.UndefOr[Boolean] = js.undefined
  var roundPixels: js.UndefOr[Boolean] = js.undefined
  var legacy: js.UndefOr[Boolean] = js.undefined
  var width: js.UndefOr[Double] = js.undefined
  var height: js.UndefOr[Double] = js.undefined
}

@js.native
@JSGlobal("PIXI.WebGLRenderer")
class WebGLRenderer () extends SystemRenderer {
  def this(options: WebGLRendererOptions) = this()
  def this(screenWidth: Double = ???, screenHeight: Double = ???, options: WebGLRendererOptions = ???) = this()
  var plugins: js.Any = js.native
  def initPlugins(): Unit = js.native
  def destroyPlugins(): Unit = js.native
  var interaction: pixijs.interaction.InteractionManager = js.native
  var maskManager: MaskManager = js.native
  var stencilManager: StencilManager = js.native
  var emptyRenderer: ObjectRenderer = js.native
  var currentRenderer: ObjectRenderer = js.native
  var gl: WebGLRenderingContext = js.native
  var CONTEXT_UID: Double = js.native
  var state: WebGLState = js.native
  var renderingToScreen: Boolean = js.native
  var boundTextures: js.Array[Texture] = js.native
  var filterManager: FilterManager = js.native
  var textureManager: TextureManager = js.native
  var textureGC: TextureGarbageCollector = js.native
  var extract: pixijs.extract.WebGLExtract = js.native
  var _activeRenderTarget: RenderTarget = js.native
  def render(displayObject: DisplayObject, renderTexture: RenderTexture = ???, clear: Boolean = ???, transform: Transform = ???, skipUpdateTransform: Boolean = ???): Unit = js.native
  def setObjectRenderer(objectRenderer: ObjectRenderer): Unit = js.native
  def flush(): Unit = js.native
  def setBlendMode(blendMode: Double): Unit = js.native
  def clear(clearColor: Double = ???): Unit = js.native
  def setTransform(matrix: Matrix): Unit = js.native
  def clearRenderTexture(renderTexture: RenderTexture, clearColor: Double = ???): WebGLRenderer = js.native
  def bindRenderTexture(renderTexture: RenderTexture, transform: Transform): WebGLRenderer = js.native
  def bindRenderTarget(renderTarget: RenderTarget): WebGLRenderer = js.native
  def bindShader(shader: Shader, autoProject: Boolean = ???): WebGLRenderer = js.native
  def bindTexture(texture: Texture | BaseTexture, location: Double = ???, forceLocation: Boolean = ???): Double = js.native
  def unbindTexture(texture: Texture | BaseTexture): WebGLRenderer | Unit = js.native
  def createVao(): glCore.VertexArrayObject = js.native
  def bindVao(vao: glCore.VertexArrayObject): WebGLRenderer = js.native
  def reset(): WebGLRenderer = js.native
  var handleContextLost: js.Function1[Event, Unit] = js.native
  var handleContextRestored: js.Function0[Unit] = js.native
}

@js.native
@JSGlobal("PIXI.WebGLRenderer")
object WebGLRenderer extends js.Object {
  var __plugins: js.Any = js.native
  def registerPlugin(pluginName: String, ctor: js.Function): Unit = js.native
}

@js.native
@JSGlobal("PIXI.WebGLState")
class WebGLState protected () extends js.Object {
  def this(gl: WebGLRenderingContext) = this()
  var activeState: js.Array[Double] = js.native
  var defaultState: js.Array[Double] = js.native
  var stackIndex: Double = js.native
  var stack: js.Array[Double] = js.native
  var gl: WebGLRenderingContext = js.native
  var maxAttribs: Double = js.native
  var attribState: glCore.AttribState = js.native
  var nativeVaoExtension: js.Any = js.native
  def push(): Unit = js.native
  def pop(): Unit = js.native
  def setState(state: js.Array[Double]): Unit = js.native
  def setBlend(value: Double): Unit = js.native
  def setBlendMode(value: Double): Unit = js.native
  def setDepthTest(value: Double): Unit = js.native
  def setCullFace(value: Double): Unit = js.native
  def setFrontFace(value: Double): Unit = js.native
  def resetAttributes(): Unit = js.native
  def resetToDefault(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.TextureManager")
class TextureManager protected () extends js.Object {
  def this(renderer: WebGLRenderer) = this()
  var renderer: WebGLRenderer = js.native
  var gl: WebGLRenderingContext = js.native
  def bindTexture(): Unit = js.native
  def getTexture(): WebGLTexture = js.native
  def updateTexture(texture: BaseTexture | Texture): WebGLTexture = js.native
  def destroyTexture(texture: BaseTexture, _skipRemove: Boolean = ???): Unit = js.native
  def removeAll(): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.TextureGarbageCollector")
class TextureGarbageCollector protected () extends js.Object {
  def this(renderer: WebGLRenderer) = this()
  var renderer: WebGLRenderer = js.native
  var count: Double = js.native
  var checkCount: Double = js.native
  var maxIdle: Double = js.native
  var checkCountMax: Double = js.native
  var mode: Double = js.native
  def update(): Unit = js.native
  def run(): Unit = js.native
  def unload(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.ObjectRenderer")
class ObjectRenderer protected () extends WebGLManager {
  def this(renderer: WebGLRenderer) = this()
  def start(): Unit = js.native
  def stop(): Unit = js.native
  def flush(): Unit = js.native
  def render(args: js.Any*): Unit = js.native
}

@js.native
@JSGlobal("PIXI.Quad")
class Quad protected () extends js.Object {
  def this(gl: WebGLRenderingContext) = this()
  var gl: WebGLRenderingContext = js.native
  var vertices: js.Array[Double] = js.native
  var uvs: js.Array[Double] = js.native
  var interleaved: js.Array[Double] = js.native
  var indices: js.Array[Double] = js.native
  var vertexBuffer: WebGLBuffer = js.native
  var vao: glCore.VertexArrayObject = js.native
  def initVao(shader: glCore.GLShader): Unit = js.native
  def map(targetTextureFrame: Rectangle, destinationFrame: Rectangle): Quad = js.native
  def upload(): Quad = js.native
  def destroy(): Unit = js.native
}

@js.native
trait FilterDataStackItem extends js.Object {
  var renderTarget: RenderTarget = js.native
  var filter: js.Array[js.Any] = js.native
  var bounds: Rectangle = js.native
}

@js.native
@JSGlobal("PIXI.RenderTarget")
class RenderTarget protected () extends js.Object {
  def this(gl: WebGLRenderingContext, width: Double, height: Double, scaleMode: Double, resolution: Double, root: Boolean = ???) = this()
  var gl: WebGLRenderingContext = js.native
  var frameBuffer: glCore.GLFramebuffer = js.native
  var texture: Texture = js.native
  var clearColor: js.Array[Double] = js.native
  var size: Rectangle = js.native
  var resolution: Double = js.native
  var projectionMatrix: Matrix = js.native
  var transform: Matrix = js.native
  var frame: Rectangle = js.native
  var defaultFrame: Rectangle = js.native
  var destinationFrame: Rectangle = js.native
  var sourceFrame: Rectangle = js.native
  var stencilBuffer: glCore.GLFramebuffer = js.native
  var stencilMaskStack: js.Array[Graphics] = js.native
  var filterData: js.Any = js.native
  var scaleMode: Double = js.native
  var root: Boolean = js.native
  def clear(clearColor: js.Array[Double] = ???): Unit = js.native
  def attachStencilBuffer(): Unit = js.native
  def setFrame(destinationFrame: Rectangle, sourceFrame: Rectangle): Unit = js.native
  def activate(): Unit = js.native
  def calculateProjection(destinationFrame: Rectangle, sourceFrame: Rectangle): Unit = js.native
  def resize(width: Double, height: Double): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.BlendModeManager")
class BlendModeManager protected () extends WebGLManager {
  def this(renderer: WebGLRenderer) = this()
  var currentBlendMode: Double = js.native
  def setBlendMode(blendMode: Double): Boolean = js.native
}

@js.native
trait FilterManagerStackItem extends js.Object {
  var renderTarget: RenderTarget = js.native
  var sourceFrame: Rectangle = js.native
  var destinationFrame: Rectangle = js.native
  var filters: js.Array[Filter] = js.native
  var target: js.Any = js.native
  var resolution: Double = js.native
}

@js.native
@JSGlobal("PIXI.FilterManager")
class FilterManager protected () extends WebGLManager {
  def this(renderer: WebGLRenderer) = this()
  var gl: WebGLRenderingContext = js.native
  var quad: Quad = js.native
  var stack: js.Array[FilterManagerStackItem] = js.native
  var stackIndex: Double = js.native
  var shaderCache: js.Any = js.native
  var filterData: js.Any = js.native
  def pushFilter(target: RenderTarget, filters: js.Array[Filter]): Unit = js.native
  def popFilter(): Unit = js.native
  def applyFilter(shader: glCore.GLShader | Filter, inputTarget: RenderTarget, outputTarget: RenderTarget, clear: Boolean = ???): Unit = js.native
  def syncUniforms(shader: glCore.GLShader, filter: Filter): Unit = js.native
  def getRenderTarget(clear: Boolean = ???, resolution: Double = ???): RenderTarget = js.native
  def returnRenderTarget(renderTarget: RenderTarget): RenderTarget = js.native
  def calculateScreenSpaceMatrix(outputMatrix: Matrix): Matrix = js.native
  def calculateNormalizedScreenSpaceMatrix(outputMatrix: Matrix): Matrix = js.native
  def calculateSpriteMatrix(outputMatrix: Matrix, sprite: Sprite): Matrix = js.native
  def emptyPool(): Unit = js.native
  def getPotRenderTarget(gl: WebGLRenderingContext, minWidth: Double, minHeight: Double, resolution: Double): RenderTarget = js.native
  def freePotRenderTarget(renderTarget: RenderTarget): Unit = js.native
}

@js.native
@JSGlobal("PIXI.StencilMaskStack")
class StencilMaskStack extends js.Object {
  var stencilStack: js.Array[js.Any] = js.native
  var reverse: Boolean = js.native
  var count: Double = js.native
}

@js.native
@JSGlobal("PIXI.MaskManager")
class MaskManager extends WebGLManager {
  var scissor: Boolean = js.native
  var scissorData: js.Any = js.native
  var scissorRenderTarget: RenderTarget = js.native
  var enableScissor: Boolean = js.native
  var alphaMaskPool: js.Array[Double] = js.native
  var alphaMaskIndex: Double = js.native
  def pushMask(target: RenderTarget, maskData: Sprite | Graphics): Unit = js.native
  def popMask(target: RenderTarget, maskData: Sprite | Graphics): Unit = js.native
  def pushSpriteMask(target: RenderTarget, maskData: Sprite | Graphics): Unit = js.native
  def popSpriteMask(): Unit = js.native
  def pushStencilMask(maskData: Sprite | Graphics): Unit = js.native
  def popStencilMask(): Unit = js.native
  def pushScissorMask(target: RenderTarget, maskData: Sprite | Graphics): Unit = js.native
  def popScissorMask(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.StencilManager")
class StencilManager protected () extends WebGLManager {
  def this(renderer: WebGLRenderer) = this()
  var stencilMaskStack: js.Array[Graphics] = js.native
  def setMaskStack(stencilMasStack: js.Array[Graphics]): Unit = js.native
  def pushStencil(graphics: Graphics): Unit = js.native
  def popStencil(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.WebGLManager")
class WebGLManager protected () extends js.Object {
  def this(renderer: WebGLRenderer) = this()
  var renderer: SystemRenderer = js.native
  def onContextChange(): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
trait UniformData extends js.Object {
  var `type`: String = js.native
  var value: js.Any = js.native
  var name: String = js.native
}

@js.native
@JSGlobal("PIXI.Filter")
class Filter protected () extends js.Object {
  def this(vertexSrc: String = ???, fragmentSrc: String = ???, uniforms: js.Dictionary[UniformData] = ???) = this()
  var vertextSrc: String = js.native
  var fragmentSrc: String = js.native
  var blendMode: Double = js.native
  var uniforms: js.Dictionary[js.Any] | js.Any = js.native
  var glShaders: js.Any = js.native
  var glShaderKey: Double = js.native
  var padding: Double = js.native
  var resolution: Double = js.native
  var enabled: Boolean = js.native
  var autoFit: Boolean = js.native
  @JSName("apply")
  def apply(filterManager: FilterManager, input: RenderTarget, output: RenderTarget, clear: Boolean = ???, currentState: js.Any = ???): Unit = js.native
}

@js.native
@JSGlobal("PIXI.Filter")
object Filter extends js.Object {
  var defaultVertexSrc: String = js.native
  var defaultFragmentSrc: String = js.native
}

@js.native
@JSGlobal("PIXI.SpriteMaskFilter")
class SpriteMaskFilter protected () extends Filter {
  def this(sprite: Sprite) = this()
  var maskSprite: Sprite = js.native
  var maskMatrix: Matrix = js.native
  @JSName("apply")
  def apply(filterManager: FilterManager, input: RenderTarget, output: RenderTarget): Unit = js.native
}

@js.native
@JSGlobal("PIXI.Sprite")
class Sprite protected () extends Container {
  def this(texture: Texture = ???) = this()
  var anchor: ObservablePoint = js.native
  var tint: Double = js.native
  var blendMode: Double = js.native
  var pluginName: String = js.native
  var texture: Texture = js.native
  var vertexData: Float32Array = js.native
  def calculateVertices(): Unit = js.native
  def getLocalBounds(): Rectangle = js.native
  def containsPoint(point: Point): Boolean = js.native
}

@js.native
@JSGlobal("PIXI.Sprite")
object Sprite extends js.Object {
  def from(source: Double | String | BaseTexture | HTMLImageElement | HTMLCanvasElement | HTMLVideoElement): Sprite = js.native
  def fromFrame(frameId: String): Sprite = js.native
  def fromImage(imageId: String, crossorigin: Boolean = ???, scaleMode: Double = ???): Sprite = js.native
}

@js.native
@JSGlobal("PIXI.BatchBuffer")
class BatchBuffer extends js.Object {
  var vertices: ArrayBuffer = js.native
  var float32View: js.Array[Double] = js.native
  var uint32View: js.Array[Double] = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.SpriteRenderer")
class SpriteRenderer protected () extends ObjectRenderer {
  def this(renderer: WebGLRenderer) = this()
  var vertSize: Double = js.native
  var vertByteSize: Double = js.native
  var size: Double = js.native
  var buffers: js.Array[BatchBuffer] = js.native
  var indices: js.Array[Double] = js.native
  var shaders: js.Array[Shader] = js.native
  var currentIndex: Double = js.native
  var tick: Double = js.native
  var groups: js.Array[js.Any] = js.native
  var sprites: js.Array[Sprite] = js.native
  var vertexBuffers: js.Array[Double] = js.native
  var vaos: js.Array[glCore.VertexArrayObject] = js.native
  var vaoMax: Double = js.native
  var vertexCount: Double = js.native
  def render(sprite: Sprite): Unit = js.native
}

@js.native
@JSGlobal("PIXI.CanvasSpriteRenderer")
class CanvasSpriteRenderer protected () extends ObjectRenderer {
  def this(renderer: WebGLRenderer) = this()
  def render(sprite: Sprite): Unit = js.native
}

package CanvasTinter {

@JSGlobal("PIXI.CanvasTinter")
@js.native
object CanvasTinter extends js.Object {
  def getTintedTexture(sprite: Sprite, color: Double): HTMLCanvasElement = js.native
  def tintWithMultiply(texture: Texture, color: Double, canvas: HTMLCanvasElement): Unit = js.native
  def tintWithOverlay(texture: Texture, color: Double, canvas: HTMLCanvasElement): Unit = js.native
  def tintWithPerPixel(texture: Texture, color: Double, canvas: HTMLCanvasElement): Unit = js.native
  def roundColor(color: Double): Double = js.native
  var cacheStepsPerColorChannel: Double = js.native
  var convertTintToImage: Boolean = js.native
  var canUseMultiply: Boolean = js.native
  var tintMethod: Double = js.native
}

}

@ScalaJSDefined
trait TextStyleOptions extends js.Object {
  var align: js.UndefOr[String] = js.undefined
  var breakWords: js.UndefOr[Boolean] = js.undefined
  var dropShadow: js.UndefOr[Boolean] = js.undefined
  var dropShadowAlpha: js.UndefOr[Double] = js.undefined
  var dropShadowAngle: js.UndefOr[Double] = js.undefined
  var dropShadowBlur: js.UndefOr[Double] = js.undefined
  var dropShadowColor: js.UndefOr[String | Double] = js.undefined
  var dropShadowDistance: js.UndefOr[Double] = js.undefined
  var fill: js.UndefOr[String | js.Array[String] | Double | js.Array[Double] | CanvasGradient | CanvasPattern] = js.undefined
  var fillGradientType: js.UndefOr[Double] = js.undefined
  var fillGradientStops: js.UndefOr[js.Array[Double]] = js.undefined
  var fontFamily: js.UndefOr[String | js.Array[String]] = js.undefined
  var fontSize: js.UndefOr[Double | String] = js.undefined
  var fontStyle: js.UndefOr[String] = js.undefined
  var fontVariant: js.UndefOr[String] = js.undefined
  var fontWeight: js.UndefOr[String] = js.undefined
  var letterSpacing: js.UndefOr[Double] = js.undefined
  var lineHeight: js.UndefOr[Double] = js.undefined
  var lineJoin: js.UndefOr[String] = js.undefined
  var miterLimit: js.UndefOr[Double] = js.undefined
  var padding: js.UndefOr[Double] = js.undefined
  var stroke: js.UndefOr[String | Double] = js.undefined
  var strokeThickness: js.UndefOr[Double] = js.undefined
  var textBaseline: js.UndefOr[String] = js.undefined
  var trim: js.UndefOr[Boolean] = js.undefined
  var wordWrap: js.UndefOr[Boolean] = js.undefined
  var wordWrapWidth: js.UndefOr[Double] = js.undefined
}

@js.native
@JSGlobal("PIXI.TextStyle")
class TextStyle protected () extends TextStyleOptions {
  def this(style: TextStyleOptions) = this()
  var styleID: Double = js.native
  @JSName("clone")
  def clone_(): TextStyle = js.native
  def reset(): Unit = js.native
  def toFontString(): String = js.native
}

@js.native
@JSGlobal("PIXI.TextMetrics")
class TextMetrics protected () extends js.Object {
  def this(text: String, style: TextStyle, width: Double, height: Double, lines: js.Array[Double], lineWidths: js.Array[Double], lineHeight: Double, maxLineWidth: Double, fontProperties: js.Any) = this()
  var text: String = js.native
  var style: TextStyle = js.native
  var width: Double = js.native
  var height: Double = js.native
  var lines: js.Array[Double] = js.native
  var lineWidgets: js.Array[Double] = js.native
  var lineHeight: Double = js.native
  var maxLineWidth: Double = js.native
  var fontProperties: js.Any = js.native
}

@js.native
@JSGlobal("PIXI.TextMetrics")
object TextMetrics extends js.Object {
  def measureText(text: String, style: TextStyle, wordWrap: Boolean = ???, canvas: HTMLCanvasElement = ???): TextMetrics = js.native
  def wordWrap(text: String, style: TextStyle, canvas: HTMLCanvasElement = ???): String = js.native
  def measureFont(font: String): FontMetrics = js.native
}

@js.native
trait FontMetrics extends js.Object {
  var ascent: Double = js.native
  var descent: Double = js.native
  var fontSize: Double = js.native
}

@js.native
@JSGlobal("PIXI.Text")
class Text protected () extends Sprite {
  def this(text: String = ???, style: TextStyleOptions = ???, canvas: HTMLCanvasElement = ???) = this()
  var canvas: HTMLCanvasElement = js.native
  var context: CanvasRenderingContext2D = js.native
  var resolution: Double = js.native
  var style: TextStyle = js.native
  var text: String = js.native
  var dirty: Boolean = js.native
}

@js.native
@JSGlobal("PIXI.BaseRenderTexture")
class BaseRenderTexture protected () extends BaseTexture {
  def this(width: Double = ???, height: Double = ???, scaleMode: Double = ???, resolution: Double = ???) = this()
  var valid: Boolean = js.native
  def resize(width: Double, height: Double): Unit = js.native
}

@js.native
@JSGlobal("PIXI.BaseTexture")
class BaseTexture protected () extends utils.EventEmitter {
  def this(source: HTMLImageElement | HTMLCanvasElement | HTMLVideoElement = ???, scaleMode: Double = ???, resolution: Double = ???) = this()
  var resolution: Double = js.native
  var width: Double = js.native
  var height: Double = js.native
  var realWidth: Double = js.native
  var realHeight: Double = js.native
  var scaleMode: Double = js.native
  var hasLoaded: Boolean = js.native
  var isLoading: Boolean = js.native
  var wrapMode: Double = js.native
  var source: HTMLImageElement | HTMLCanvasElement | HTMLVideoElement | Null = js.native
  var origSource: HTMLImageElement | Null = js.native
  var imageType: String | Null = js.native
  var sourceScale: Double = js.native
  var premultipliedAlpha: Boolean = js.native
  var imageUrl: String | Null = js.native
  var mipmap: Boolean = js.native
  var wrap: Boolean = js.native
  var textureCacheIds: js.Array[String] = js.native
  def update(): Unit = js.native
  def destroy(): Unit = js.native
  def dispose(): Unit = js.native
  def updateSourceImage(newSrc: String): Unit = js.native
}

@js.native
@JSGlobal("PIXI.BaseTexture")
object BaseTexture extends js.Object {
  def from(source: String | HTMLImageElement | HTMLCanvasElement, scaleMode: Double = ???, sourceScale: Double = ???): BaseTexture = js.native
  def fromImage(imageUrl: String, crossorigin: Boolean = ???, scaleMode: Double = ???, sourceScale: Double = ???): BaseTexture = js.native
  def fromCanvas(canvas: HTMLCanvasElement, scaleMode: Double = ???, origin: String = ???): BaseTexture = js.native
  def addToCache(baseTexture: BaseTexture, id: String): Unit = js.native
  def removeFromCache(baseTexture: String | BaseTexture): BaseTexture = js.native
}

@js.native
@JSGlobal("PIXI.RenderTexture")
class RenderTexture protected () extends Texture {
  def this(baseRenderTexture: BaseRenderTexture, frame: Rectangle = ???) = this()
  def resize(width: Double, height: Double, doNotResizeBaseTexture: Boolean = ???): Unit = js.native
}

@js.native
@JSGlobal("PIXI.RenderTexture")
object RenderTexture extends js.Object {
  def create(width: Double = ???, height: Double = ???, scaleMode: Double = ???, resolution: Double = ???): RenderTexture = js.native
}

@js.native
@JSGlobal("PIXI.Texture")
class Texture protected () extends utils.EventEmitter {
  def this(baseTexture: BaseTexture, frame: Rectangle = ???, orig: Rectangle = ???, trim: Rectangle = ???, rotate: Double = ???) = this()
  var noFrame: Boolean = js.native
  var baseTexture: BaseTexture = js.native
  var trim: Rectangle = js.native
  var valid: Boolean = js.native
  var requiresUpdate: Boolean = js.native
  var orig: Rectangle = js.native
  var transform: js.Any = js.native
  var textureCacheIds: js.Array[String] = js.native
  def update(): Unit = js.native
  def destroy(destroyBase: Boolean = ???): Unit = js.native
  @JSName("clone")
  def clone_(): Texture = js.native
  var frame: Rectangle = js.native
  var rotate: Double = js.native
  var width: Double = js.native
  var height: Double = js.native
}

@js.native
@JSGlobal("PIXI.Texture")
object Texture extends js.Object {
  def fromImage(imageUrl: String, crossOrigin: Boolean = ???, scaleMode: Double = ???, sourceScale: Double = ???): Texture = js.native
  def fromFrame(frameId: String): Texture = js.native
  def fromCanvas(canvas: HTMLCanvasElement, scaleMode: Double = ???, origin: String = ???): Texture = js.native
  def fromVideo(video: HTMLVideoElement | String, scaleMode: Double = ???): Texture = js.native
  def fromVideoUrl(videoUrl: String, scaleMode: Double = ???): Texture = js.native
  def from(source: Double | String | HTMLImageElement | HTMLCanvasElement | HTMLVideoElement | BaseTexture): Texture = js.native
  def fromLoader(source: HTMLImageElement | HTMLCanvasElement, imageUrl: String, name: String = ???): Texture = js.native
  def addToCache(texture: Texture, id: String): Unit = js.native
  def removeFromCache(texture: String | Texture): Texture = js.native
  def addTextureToCache(texture: Texture, id: String): Unit = js.native
  def removeTextureFromCache(id: String): Texture = js.native
  var EMPTY: Texture = js.native
  var WHITE: Texture = js.native
}

@js.native
@JSGlobal("PIXI.TextureUvs")
class TextureUvs extends js.Object {
  var x0: Double = js.native
  var y0: Double = js.native
  var x1: Double = js.native
  var y1: Double = js.native
  var x2: Double = js.native
  var y2: Double = js.native
  var x3: Double = js.native
  var y3: Double = js.native
  var uvsUint32: Uint32Array = js.native
}

@js.native
@JSGlobal("PIXI.Spritesheet")
class Spritesheet protected () extends js.Object {
  def this(baseTexture: BaseTexture, data: js.Any, resolutionFilename: String = ???) = this()
  var baseTexture: BaseTexture = js.native
  var textures: js.Dictionary[Texture] = js.native
  var data: js.Any = js.native
  var resolution: Double = js.native
  def parse(callback: js.Function2[this.type, js.Dictionary[Texture], Unit]): Unit = js.native
  def destroy(destroyBase: Boolean = ???): Unit = js.native
}

@js.native
@JSGlobal("PIXI.Spritesheet")
object Spritesheet extends js.Object {
  var BATCH_SIZE: Double = js.native
}

@js.native
@JSGlobal("PIXI.VideoBaseTexture")
class VideoBaseTexture protected () extends BaseTexture {
  def this(source: HTMLVideoElement, scaleMode: Double = ???) = this()
  var autoUpdate: Boolean = js.native
  var autoPlay: Boolean = js.native
}

@js.native
@JSGlobal("PIXI.VideoBaseTexture")
object VideoBaseTexture extends js.Object {
  def fromVideo(video: HTMLVideoElement, scaleMode: Double = ???): VideoBaseTexture = js.native
  def fromUrl(videoSrc: String | js.Any | js.Array[String] | js.Array[js.Any]): VideoBaseTexture = js.native
  def fromUrls(videoSrc: String | js.Any | js.Array[String] | js.Array[js.Any]): VideoBaseTexture = js.native
}

package ticker {

@js.native
@JSGlobal("PIXI.ticker.TickerListener")
class TickerListener protected () extends js.Object {
  def this(fn: js.Function1[Double, Unit], context: js.Any = ???, priority: Double = ???, once: Boolean = ???) = this()
  var fn: js.Function1[Double, Unit] = js.native
  var context: js.Any = js.native
  var priority: Double = js.native
  var once: Boolean = js.native
  var next: TickerListener = js.native
  var previous: TickerListener = js.native
  def `match`(fn: js.Function1[Double, Unit], context: js.Any = ???): Boolean = js.native
  def emit(deltaTime: Double): TickerListener = js.native
  def connect(previous: TickerListener): Unit = js.native
  def destroy(hard: Boolean = ???): Unit = js.native
}

@js.native
@JSGlobal("PIXI.ticker.Ticker")
class Ticker extends js.Object {
  var autoStart: Boolean = js.native
  var deltaTime: Double = js.native
  var elapsedMS: Double = js.native
  var lastTime: Double = js.native
  var speed: Double = js.native
  var started: Boolean = js.native
  def add(fn: js.Function1[Double, Unit], context: js.Any = ???, priority: Double = ???): Ticker = js.native
  def addOnce(fn: js.Function1[Double, Unit], context: js.Any = ???, priority: Double = ???): Ticker = js.native
  def remove(fn: js.Function, context: js.Any = ???, priority: Double = ???): Ticker = js.native
  var minFPS: Double = js.native
  def start(): Unit = js.native
  def stop(): Unit = js.native
  def destroy(): Unit = js.native
  def update(currentTime: Double = ???): Unit = js.native
}

@JSGlobal("PIXI.ticker")
@js.native
object Ticker extends js.Object {
  val shared: Ticker = js.native
}

}

@js.native
@JSGlobal("PIXI.Shader")
class Shader extends glCore.GLShader {
}

package extract {

@js.native
@JSGlobal("PIXI.extract.CanvasExtract")
class CanvasExtract protected () extends js.Object {
  def this(renderer: CanvasRenderer) = this()
  def image(target: DisplayObject | RenderTexture = ???): HTMLImageElement = js.native
  def base64(target: DisplayObject | RenderTexture = ???): String = js.native
  def canvas(target: DisplayObject | RenderTexture = ???): HTMLCanvasElement = js.native
  def pixels(renderTexture: DisplayObject | RenderTexture = ???): js.Array[Double] = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.extract.WebGLExtract")
class WebGLExtract protected () extends js.Object {
  def this(renderer: WebGLRenderer) = this()
  def image(target: DisplayObject | RenderTexture = ???): HTMLImageElement = js.native
  def base64(target: DisplayObject | RenderTexture = ???): String = js.native
  def canvas(target: DisplayObject | RenderTexture = ???): HTMLCanvasElement = js.native
  def pixels(renderTexture: DisplayObject | RenderTexture = ???): js.Array[Double] = js.native
  def destroy(): Unit = js.native
}

}

package extras {

@js.native
trait BitmapTextStyle extends js.Object {
  var font: String | js.Any = js.native
  var align: String = js.native
  var tint: Double = js.native
}

@js.native
@JSGlobal("PIXI.extras.BitmapText")
class BitmapText protected () extends Container {
  def this(text: String, style: BitmapTextStyle = ???) = this()
  var textWidth: Double = js.native
  var textHeight: Double = js.native
  var font: String | js.Any = js.native
  var maxWidth: Double = js.native
  var maxLineHeight: Double = js.native
  var dirty: Boolean = js.native
  var tint: Double = js.native
  var align: String = js.native
  var text: String = js.native
  var anchor: Point | Double = js.native
  def getLocalBounds(): Rectangle = js.native
}

@js.native
@JSGlobal("PIXI.extras.BitmapText")
object BitmapText extends js.Object {
  def registerFont(xml: js.Any, texture: Texture): js.Dynamic = js.native
  var fonts: js.Any = js.native
}

@js.native
trait AnimatedSpriteTextureTimeObject extends js.Object {
  var texture: Texture = js.native
  var time: Double = js.native
}

@js.native
@JSGlobal("PIXI.extras.AnimatedSprite")
class AnimatedSprite protected () extends Sprite {
  def this(textures: js.Array[Texture] | js.Array[AnimatedSpriteTextureTimeObject], autoUpdate: Boolean = ???) = this()
  var textures: js.Array[Texture] | js.Array[AnimatedSpriteTextureTimeObject] = js.native
  var animationSpeed: Double = js.native
  var loop: Boolean = js.native
  var onComplete: js.Function0[Unit] = js.native
  var onFrameChange: js.Function1[Double, Unit] = js.native
  var playing: Boolean = js.native
  var totalFrames: Double = js.native
  var currentFrame: Double = js.native
  def stop(): Unit = js.native
  def play(): Unit = js.native
  def gotoAndStop(frameNumber: Double): Unit = js.native
  def gotoAndPlay(frameNumber: Double): Unit = js.native
}

@js.native
@JSGlobal("PIXI.extras.AnimatedSprite")
object AnimatedSprite extends js.Object {
  def fromFrames(frame: js.Array[String]): AnimatedSprite = js.native
  def fromImages(images: js.Array[String]): AnimatedSprite = js.native
}

@js.native
@JSGlobal("PIXI.extras.TextureTransform")
class TextureTransform protected () extends js.Object {
  def this(texture: Texture, clampMargin: Double = ???) = this()
  var clampOffset: Double = js.native
  var clampMargin: Double = js.native
  var texture: Texture = js.native
  def update(forceUpdate: Boolean = ???): Boolean = js.native
}

@js.native
@JSGlobal("PIXI.extras.TilingSprite")
class TilingSprite protected () extends Sprite {
  def this(texture: Texture, width: Double = ???, height: Double = ???) = this()
  var tileTransform: TransformStatic = js.native
  var uvTransform: TextureTransform = js.native
  var uvRespectAnchor: Boolean = js.native
  var clampMargin: Double = js.native
  var tileScale: Point | ObservablePoint = js.native
  var tilePosition: Point | ObservablePoint = js.native
  def multiplyUvs(uvs: Float32Array, out: Float32Array): Float32Array = js.native
}

@js.native
@JSGlobal("PIXI.extras.TilingSprite")
object TilingSprite extends js.Object {
  def from(source: Double | String | BaseTexture | HTMLCanvasElement | HTMLVideoElement, width: Double = ???, height: Double = ???): TilingSprite = js.native
  def fromFrame(frameId: String, width: Double, height: Double): TilingSprite = js.native
  def fromImage(imageId: String, crossorigin: Boolean, scaleMode: Double): Sprite = js.native
  def fromImage(imageId: String, width: Double = ???, height: Double = ???, crossorigin: Boolean = ???, scaleMode: Double = ???): TilingSprite = js.native
}

@js.native
@JSGlobal("PIXI.extras.TilingSpriteRenderer")
class TilingSpriteRenderer protected () extends ObjectRenderer {
  def this(renderer: WebGLRenderer) = this()
  def render(ts: TilingSprite): Unit = js.native
}

@JSGlobal("PIXI.extras")
@js.native
object Extras extends js.Object {
  type MovieClip = extras.AnimatedSprite
}

}

package filters {

@js.native
@JSGlobal("PIXI.filters.FXAAFilter")
class FXAAFilter extends Filter {
}

@js.native
@JSGlobal("PIXI.filters.BlurFilter")
class BlurFilter protected () extends Filter {
  def this(strength: Double = ???, quality: Double = ???, resolution: Double = ???, kernelSize: Double = ???) = this()
  var blurXFilter: BlurXFilter = js.native
  var blurYFilter: BlurYFilter = js.native
  var passes: Double = js.native
  var blur: Double = js.native
  var blurX: Double = js.native
  var blurY: Double = js.native
  var quality: Double = js.native
}

@js.native
@JSGlobal("PIXI.filters.BlurXFilter")
class BlurXFilter protected () extends Filter {
  def this(strength: Double = ???, quality: Double = ???, resolution: Double = ???, kernelSize: Double = ???) = this()
  var quality: Double = js.native
  var passes: Double = js.native
  var strength: Double = js.native
  var firstRun: Boolean = js.native
  var blur: Double = js.native
}

@js.native
@JSGlobal("PIXI.filters.BlurYFilter")
class BlurYFilter protected () extends Filter {
  def this(strength: Double = ???, quality: Double = ???, resolution: Double = ???, kernelSize: Double = ???) = this()
  var quality: Double = js.native
  var passes: Double = js.native
  var strength: Double = js.native
  var firstRun: Boolean = js.native
  var blur: Double = js.native
}

@js.native
@JSGlobal("PIXI.filters.ColorMatrixFilter")
class ColorMatrixFilter extends Filter {
  var matrix: js.Array[Double] = js.native
  var alpha: Double = js.native
  def brightness(b: Double, multiply: Boolean = ???): Unit = js.native
  def greyscale(scale: Double, multiply: Boolean = ???): Unit = js.native
  def blackAndWhite(multiply: Boolean = ???): Unit = js.native
  def hue(rotation: Double, multiply: Boolean = ???): Unit = js.native
  def contrast(amount: Double, multiply: Boolean = ???): Unit = js.native
  def saturate(amount: Double, multiply: Boolean = ???): Unit = js.native
  def desaturate(multiply: Boolean = ???): Unit = js.native
  def negative(multiply: Boolean = ???): Unit = js.native
  def sepia(multiply: Boolean = ???): Unit = js.native
  def technicolor(multiply: Boolean = ???): Unit = js.native
  def polaroid(multiply: Boolean = ???): Unit = js.native
  def toBGR(multiply: Boolean = ???): Unit = js.native
  def kodachrome(multiply: Boolean = ???): Unit = js.native
  def browni(multiply: Boolean = ???): Unit = js.native
  def vintage(multiply: Boolean = ???): Unit = js.native
  def colorTone(desaturation: Double, toned: Double, lightColor: String, darkColor: String, multiply: Boolean = ???): Unit = js.native
  def night(intensity: Double, multiply: Boolean = ???): Unit = js.native
  def predator(amount: Double, multiply: Boolean = ???): Unit = js.native
  def lsd(multiply: Boolean = ???): Unit = js.native
  def reset(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.filters.DisplacementFilter")
class DisplacementFilter protected () extends Filter {
  def this(sprite: Sprite, scale: Double = ???) = this()
  var scale: Point = js.native
  var map: Texture = js.native
}

@js.native
@JSGlobal("PIXI.filters.VoidFilter")
class VoidFilter extends Filter {
}

@js.native
@JSGlobal("PIXI.filters.NoiseFilter")
class NoiseFilter protected () extends Filter {
  def this(noise: Double = ???, seed: Double = ???) = this()
  var noise: Double = js.native
  var seed: Double = js.native
}

}

package interaction {

@js.native
trait InteractiveTarget extends js.Object {
  var interactive: Boolean = js.native
  var interactiveChildren: Boolean = js.native
  var hitArea: Rectangle | Circle | Ellipse | Polygon | RoundedRectangle = js.native
  var buttonMode: Boolean = js.native
  var cursor: String = js.native
  def trackedPointers(): js.Any = js.native
  var defaultCursor: String = js.native
}

@js.native
trait InteractionTrackingData extends js.Object {
  var flags: Double = js.native
  var none: Double = js.native
  var over: Boolean = js.native
  var rightDown: Boolean = js.native
  var leftDown: Boolean = js.native
}

@js.native
trait InteractionEvent extends js.Object {
  var stopped: Boolean = js.native
  var target: DisplayObject = js.native
  var currentTarget: DisplayObject = js.native
  var `type`: String = js.native
  var data: InteractionData = js.native
  def stopPropagation(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.interaction.InteractionData")
class InteractionData extends js.Object {
  var global: Point = js.native
  var target: DisplayObject = js.native
  var originalEvent: MouseEvent | TouchEvent = js.native
  var identifier: Double = js.native
  var isPrimary: Boolean = js.native
  var button: Double = js.native
  var buttons: Double = js.native
  var width: Double = js.native
  var height: Double = js.native
  var tiltX: Double = js.native
  var tiltY: Double = js.native
  var pointerType: String = js.native
  var pressure: Double = js.native
  var rotationAngle: Double = js.native
  var twist: Double = js.native
  var tangentialPressure: Double = js.native
  def getLocalPosition(displayObject: DisplayObject, point: Point = ???, globalPos: Point = ???): Point = js.native
}

@ScalaJSDefined
trait InteractionManagerOptions extends js.Object {
  var autoPreventDefault: js.UndefOr[Boolean] = js.undefined
  var interactionFrequency: js.UndefOr[Double]= js.undefined
}

@js.native
@JSGlobal("PIXI.interaction.InteractionManager")
class InteractionManager protected () extends utils.EventEmitter {
  def this(renderer: CanvasRenderer | WebGLRenderer | SystemRenderer, options: InteractionManagerOptions = ???) = this()
  var renderer: SystemRenderer = js.native
  var autoPreventDefault: Boolean = js.native
  var interactionFrequency: Double = js.native
  var mouse: InteractionData = js.native
  var activeInteractionData: js.Any = js.native
  var interactionDataPool: js.Array[InteractionData] = js.native
  var eventData: InteractionEvent = js.native
  var moveWhenInside: Boolean = js.native
  var eventsAdded: Boolean = js.native
  var cursorStyles: js.Any = js.native
  var currentCursorMode: String = js.native
  var cursor: String = js.native
  var resolution: Double = js.native
  def hitTest(globalPoint: Point, root: Container = ???): DisplayObject = js.native
  def update(deltaTime: Double = ???): Unit = js.native
  def setCursorMode(mode: String): Unit = js.native
  def mapPositionToPoint(point: Point, x: Double, y: Double): Unit = js.native
  def destroy(): Unit = js.native
  var defaultCursorStyle: String = js.native
  var currentCursorStyle: String = js.native
}

@JSGlobal("PIXI.interaction")
@js.native
object Interaction extends js.Object {
  type InteractionPointerEvents = js.Any | js.Any | js.Any | js.Any | js.Any | js.Any | js.Any | js.Any
  type InteractionTouchEvents = js.Any | js.Any | js.Any | js.Any | js.Any | js.Any
  type InteractionMouseEvents = js.Any | js.Any | js.Any | js.Any | js.Any | js.Any | js.Any | js.Any | js.Any | js.Any | js.Any | js.Any
  type InteractionEventTypes = InteractionPointerEvents | InteractionTouchEvents | InteractionMouseEvents
}

}

@js.native
@JSGlobal("PIXI.MiniSignalBinding")
class MiniSignalBinding protected () extends js.Object {
  def this(fn: js.Function, once: Boolean = ???, thisArg: js.Any = ???) = this()
  def detach(): Boolean = js.native
}

@js.native
@JSGlobal("PIXI.MiniSignal")
class MiniSignal extends js.Object {
  def handlers(exists: Boolean = ???): js.Array[MiniSignalBinding] | Boolean = js.native
  def has(node: MiniSignalBinding): Boolean = js.native
  def dispatch(): Boolean = js.native
  def add(fn: js.Function, thisArg: js.Any = ???): js.Dynamic = js.native
  def once(fn: js.Function, thisArg: js.Any = ???): js.Dynamic = js.native
  def detach(node: MiniSignalBinding): MiniSignal = js.native
  def detachAll(): MiniSignal = js.native
}

package loaders {

@js.native
trait LoaderOptions extends js.Object {
  var crossOrigin: js.UndefOr[Boolean | String] = js.undefined
  var loadType: js.UndefOr[Double] = js.undefined
  var xhrType: js.UndefOr[String] = js.undefined
  var metaData: js.UndefOr[js.Any] = js.undefined
  var loadElement: js.UndefOr[HTMLImageElement | HTMLAudioElement | HTMLVideoElement] = js.undefined
  var skipSource: js.UndefOr[Boolean] = js.undefined
}

@js.native
trait ResourceDictionary extends js.Object {
  @JSBracketAccess
  def apply(index: String): loaders.Resource = js.native
  @JSBracketAccess
  def update(index: String, v: loaders.Resource): Unit = js.native
}

@js.native
@JSGlobal("PIXI.loaders.Loader")
class Loader protected () extends utils.EventEmitter {
  def this(baseUrl: String = ???, concurrency: Double = ???) = this()
  var baseUrl: String = js.native
  var progress: Double = js.native
  var loading: Boolean = js.native
  var defaultQueryString: String = js.native
  var resources: ResourceDictionary = js.native
  var onProgress: MiniSignal = js.native
  var onError: MiniSignal = js.native
  var onLoad: MiniSignal = js.native
  var onStart: MiniSignal = js.native
  var onComplete: MiniSignal = js.native
  def add(params: js.Any*): this.type = js.native
  //def add(name: String, url: String, options: LoaderOptions = ???, cb: js.Function = ???): this.type = js.native
  //def add(obj: String | js.Any | js.Array[js.Any], options: LoaderOptions = ???, cb: js.Function = ???): this.type = js.native
  def pre(fn: js.Function): this.type = js.native
  def use(fn: js.Function): this.type = js.native
  def reset(): this.type = js.native
  def load(cb: js.Function = ???): this.type = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.loaders.Loader")
object Loader extends js.Object {
  def addPixiMiddleware(fn: js.Function): Unit = js.native
  var Resource: js.Any = js.native
  var async: js.Any = js.native
  var base64: js.Any = js.native
}

@js.native
trait TextureDictionary extends js.Object {
  @JSBracketAccess
  def apply(index: String): Texture = js.native
  @JSBracketAccess
  def update(index: String, v: Texture): Unit = js.native
}

@js.native
@JSGlobal("PIXI.loaders.Resource")
class Resource protected () extends js.Object {
  def this(name: String, url: String | js.Array[String], options: LoaderOptions = ???) = this()
  var name: String = js.native
  var url: String = js.native
  var extension: String = js.native
  var data: js.Any = js.native
  var crossOrigin: Boolean | String = js.native
  var loadType: Double = js.native
  var xhrType: String = js.native
  var metadata: js.Any = js.native
  var error: Error = js.native
  var xhr: XMLHttpRequest | Null = js.native
  var children: js.Array[Resource] = js.native
  var `type`: Double = js.native
  var progressChunk: Double = js.native
  var onStart: MiniSignal = js.native
  var onProgress: MiniSignal = js.native
  var onComplete: MiniSignal = js.native
  var onAfterMiddleware: MiniSignal = js.native
  var isDataUrl: Boolean = js.native
  var isComplete: Boolean = js.native
  var isLoading: Boolean = js.native
  def complete(): Unit = js.native
  def abort(message: String = ???): Unit = js.native
  def load(cb: js.Function = ???): Unit = js.native
  var texture: Texture = js.native
  var spineAtlas: js.Any = js.native
  var spineData: js.Any = js.native
  var textures: TextureDictionary = js.native
}

@js.native
@JSGlobal("PIXI.loaders.Resource")
object Resource extends js.Object {
  def setExtensionLoadType(extname: String, loadType: Double): Unit = js.native
  def setExtensionXhrType(extname: String, xhrType: String): Unit = js.native
  var STATUS_FLAGS: js.Any = js.native
  var TYPE: js.Any = js.native
  var LOAD_TYPE: js.Any = js.native
  var XHR_RESPONSE_TYPE: js.Any = js.native
  var EMPTY_GIF: String = js.native
}

}

package mesh {

@js.native
@JSGlobal("PIXI.mesh.Mesh")
class Mesh protected () extends Container {
  def this(texture: Texture, vertices: Float32Array = ???, uvs: Float32Array = ???, indices: Uint16Array = ???, drawMode: Double = ???) = this()
  var uvs: Float32Array = js.native
  var vertices: Float32Array = js.native
  var indices: Uint16Array = js.native
  var dirty: Double = js.native
  var indexDirty: Double = js.native
  var dirtyVertex: Boolean = js.native
  var blendMode: Double = js.native
  var pluginName: String = js.native
  var canvasPadding: Double = js.native
  var drawMode: Double = js.native
  var texture: Texture = js.native
  var tintRgb: Float32Array = js.native
  var uploadUvTransform: Boolean = js.native
  def multiplyUvs(): Unit = js.native
  def refresh(forceUpdate: Boolean = ???): Unit = js.native
  def containsPoint(point: Point): Boolean = js.native
  var tint: Double = js.native
}

@js.native
@JSGlobal("PIXI.mesh.Mesh")
object Mesh extends js.Object {
  var DRAW_MODES: js.Any = js.native
}

@js.native
@JSGlobal("PIXI.mesh.CanvasMeshRenderer")
class CanvasMeshRenderer protected () extends js.Object {
  def this(renderer: CanvasRenderer) = this()
  var renderer: CanvasRenderer = js.native
  def render(mesh: Mesh): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.mesh.MeshRenderer")
class MeshRenderer protected () extends ObjectRenderer {
  def this(renderer: WebGLRenderer) = this()
  var shader: Shader = js.native
  def render(mesh: Mesh): Unit = js.native
}

@js.native
@JSGlobal("PIXI.mesh.Plane")
class Plane protected () extends Mesh {
  def this(texture: Texture, verticesX: Double = ???, verticesY: Double = ???) = this()
  var verticesX: Double = js.native
  var verticesY: Double = js.native
  def refresh(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.mesh.NineSlicePlane")
class NineSlicePlane protected () extends Plane {
  def this(texture: Texture, leftWidth: Double = ???, topHeight: Double = ???, rightWidth: Double = ???, bottomHeight: Double = ???) = this()
  var leftWidth: Double = js.native
  var rightWidth: Double = js.native
  var topHeight: Double = js.native
  var bottomHeight: Double = js.native
  def updateHorizontalVertices(): Unit = js.native
  def updateVerticalVertices(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.mesh.Rope")
class Rope protected () extends Mesh {
  def this(texture: Texture, points: js.Array[Point]) = this()
  var points: js.Array[Point] = js.native
  var colors: js.Array[Double] = js.native
  var autoUpdate: Boolean = js.native
  def refreshVertices(): Unit = js.native
}

}

package particles {

@js.native
trait ParticleContainerProperties extends js.Object {
  var scale: Boolean = js.native
  var position: Boolean = js.native
  var rotation: Boolean = js.native
  var uvs: Boolean = js.native
  var alpha: Boolean = js.native
}

@js.native
@JSGlobal("PIXI.particles.ParticleContainer")
class ParticleContainer protected () extends Container {
  def this(size: Double = ???, properties: ParticleContainerProperties = ???, batchSize: Double = ???) = this()
  var tint: Double = js.native
  var blendMode: Double = js.native
  var roundPixels: Boolean = js.native
  var baseTexture: BaseTexture = js.native
  def setProperties(properties: ParticleContainerProperties): Unit = js.native
}

@js.native
@JSGlobal("PIXI.particles.ParticleBuffer")
class ParticleBuffer protected () extends js.Object {
  def this(gl: WebGLRenderingContext, properties: js.Any, dynamicPropertyFlags: js.Array[js.Any], size: Double) = this()
  var gl: WebGLRenderingContext = js.native
  var vertSize: Double = js.native
  var vertByteSize: Double = js.native
  var size: Double = js.native
  var dynamicProperties: js.Array[js.Any] = js.native
  var staticProperties: js.Array[js.Any] = js.native
  var staticStride: Double = js.native
  var staticBuffer: js.Any = js.native
  var staticData: js.Any = js.native
  var dynamicStride: Double = js.native
  var dynamicBuffer: js.Any = js.native
  var dynamicData: js.Any = js.native
  def destroy(): Unit = js.native
}

@js.native
trait ParticleRendererProperty extends js.Object {
  var attribute: Double = js.native
  var size: Double = js.native
  var uploadFunction: js.Function6[js.Array[DisplayObject], Double, Double, js.Array[Double], Double, Double, Unit] = js.native
  var offset: Double = js.native
}

@js.native
@JSGlobal("PIXI.particles.ParticleRenderer")
class ParticleRenderer protected () extends ObjectRenderer {
  def this(renderer: WebGLRenderer) = this()
  var shader: glCore.GLShader = js.native
  var indexBuffer: WebGLBuffer = js.native
  var properties: js.Array[ParticleRendererProperty] = js.native
  def generateBuffers(container: ParticleContainer): js.Array[ParticleBuffer] = js.native
  def uploadVertices(children: js.Array[DisplayObject], startIndex: Double, amount: Double, array: js.Array[Double], stride: Double, offset: Double): Unit = js.native
  def uploadPosition(children: js.Array[DisplayObject], startIndex: Double, amount: Double, array: js.Array[Double], stride: Double, offset: Double): Unit = js.native
  def uploadRotation(children: js.Array[DisplayObject], startIndex: Double, amount: Double, array: js.Array[Double], stride: Double, offset: Double): Unit = js.native
  def uploadUvs(children: js.Array[DisplayObject], startIndex: Double, amount: Double, array: js.Array[Double], stride: Double, offset: Double): Unit = js.native
  def uploadAlpha(children: js.Array[DisplayObject], startIndex: Double, amount: Double, array: js.Array[Double], stride: Double, offset: Double): Unit = js.native
  var indices: Uint16Array = js.native
}

}

package prepare {

@js.native
@JSGlobal("PIXI.prepare.BasePrepare")
class BasePrepare[UploadHookSource] protected () extends js.Object {
  def this(renderer: SystemRenderer) = this()
  var limiter: CountLimiter | TimeLimiter = js.native
  def upload(item: js.Function | DisplayObject | Container | BaseTexture | Texture | Graphics | Text | js.Any, done: js.Function0[Unit] = ???): Unit = js.native
  def registerFindHook(addHook: Prepare.AddHook): this.type = js.native
  def registerUploadHook(uploadHook: Prepare.UploadHook[UploadHookSource]): this.type = js.native
  def add(item: DisplayObject | Container | BaseTexture | Texture | Graphics | Text | js.Any): this.type = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.prepare.CanvasPrepare")
class CanvasPrepare protected () extends BasePrepare[CanvasPrepare] {
  def this(renderer: CanvasRenderer) = this()
}

@js.native
@JSGlobal("PIXI.prepare.WebGLPrepare")
class WebGLPrepare protected () extends BasePrepare[WebGLRenderer] {
  def this(renderer: WebGLRenderer) = this()
}

@js.native
@JSGlobal("PIXI.prepare.CountLimiter")
class CountLimiter protected () extends js.Object {
  def this(maxItemsPerFrame: Double) = this()
}

@js.native
@JSGlobal("PIXI.prepare.TimeLimiter")
class TimeLimiter protected () extends js.Object {
  def this(maxMilliseconds: Double) = this()
}

@JSGlobal("PIXI.prepare")
@js.native
object Prepare extends js.Object {
  type AddHook = js.Function2[js.Any, js.Array[js.Any], Boolean]
  type UploadHook[UploadHookSource] = js.Function2[UploadHookSource, js.Any, Boolean]
}

}

package glCore {

@js.native
trait ContextOptions extends js.Object {
  var alpha: js.UndefOr[Boolean] = js.undefined
  var depth: js.UndefOr[Boolean] = js.undefined
  var stencil: js.UndefOr[Boolean] = js.undefined
  var antialias: js.UndefOr[Boolean] = js.undefined
  var premultipliedAlpha: js.UndefOr[Boolean] = js.undefined
  var preserveDrawingBuffer: js.UndefOr[Boolean] = js.undefined
  var failIfMajorPerformanceCaveat: js.UndefOr[Boolean] = js.undefined
}

@js.native
@JSGlobal("PIXI.glCore.GLBuffer")
class GLBuffer protected () extends js.Object {
  def this(gl: WebGLRenderingContext, `type`: Double, data: ArrayBuffer | ArrayBufferView | js.Any, drawType: Double) = this()
  var gl: WebGLRenderingContext = js.native
  var buffer: WebGLBuffer = js.native
  var `type`: Double = js.native
  var drawType: Double = js.native
  var data: ArrayBuffer | ArrayBufferView | js.Any = js.native
  def upload(data: ArrayBuffer | ArrayBufferView | js.Any, offset: Double = ???, dontBind: Boolean = ???): Unit = js.native
  def bind(): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.glCore.GLBuffer")
object GLBuffer extends js.Object {
  def createVertexBuffer(gl: WebGLRenderingContext, data: ArrayBuffer | ArrayBufferView | js.Any, drawType: Double): GLBuffer = js.native
  def createIndexBuffer(gl: WebGLRenderingContext, data: ArrayBuffer | ArrayBufferView | js.Any, drawType: Double): GLBuffer = js.native
  def create(gl: WebGLRenderingContext, `type`: Double, data: ArrayBuffer | ArrayBufferView | js.Any, drawType: Double): GLBuffer = js.native
}

@js.native
@JSGlobal("PIXI.glCore.GLFramebuffer")
class GLFramebuffer protected () extends js.Object {
  def this(gl: WebGLRenderingContext, width: Double, height: Double) = this()
  var gl: WebGLRenderingContext = js.native
  var frameBuffer: WebGLFramebuffer = js.native
  var stencil: WebGLRenderbuffer = js.native
  var texture: GLTexture = js.native
  var width: Double = js.native
  var height: Double = js.native
  def enableTexture(texture: GLTexture): Unit = js.native
  def enableStencil(): Unit = js.native
  def clear(r: Double, g: Double, b: Double, a: Double): Unit = js.native
  def bind(): Unit = js.native
  def unbind(): Unit = js.native
  def resize(width: Double, height: Double): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.glCore.GLFramebuffer")
object GLFramebuffer extends js.Object {
  def createRGBA(gl: WebGLRenderingContext, width: Double, height: Double, data: ArrayBuffer | ArrayBufferView | js.Any): GLFramebuffer = js.native
  def createFloat32(gl: WebGLRenderingContext, width: Double, height: Double, data: ArrayBuffer | ArrayBufferView | js.Any): GLFramebuffer = js.native
}

@js.native
@JSGlobal("PIXI.glCore.GLShader")
class GLShader protected () extends js.Object {
  def this(gl: WebGLRenderingContext, vertexSrc: String | js.Array[String], fragmentSrc: String | js.Array[String], precision: String = ???, attributeLocations: js.Dictionary[Double] = ???) = this()
  var gl: WebGLRenderingContext = js.native
  var program: WebGLProgram | Null = js.native
  var uniformData: js.Any = js.native
  var uniforms: js.Any = js.native
  var attributes: js.Any = js.native
  def bind(): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.glCore.GLTexture")
class GLTexture protected () extends js.Object {
  def this(gl: WebGLRenderingContext, width: Double = ???, height: Double = ???, format: Double = ???, `type`: Double = ???) = this()
  var gl: WebGLRenderingContext = js.native
  var texture: WebGLTexture = js.native
  var mipmap: Boolean = js.native
  var premultiplyAlpha: Boolean = js.native
  var width: Double = js.native
  var height: Double = js.native
  var format: Double = js.native
  var `type`: Double = js.native
  def upload(source: HTMLImageElement | ImageData | HTMLVideoElement | HTMLCanvasElement): Unit = js.native
  def uploadData(data: ArrayBuffer | ArrayBufferView, width: Double, height: Double): Unit = js.native
  def bind(location: Double = ???): Unit = js.native
  def unbind(): Unit = js.native
  def minFilter(linear: Boolean): Unit = js.native
  def magFilter(linear: Boolean): Unit = js.native
  def enableMipmap(): Unit = js.native
  def enableLinearScaling(): Unit = js.native
  def enableNearestScaling(): Unit = js.native
  def enableWrapClamp(): Unit = js.native
  def enableWrapRepeat(): Unit = js.native
  def enableWrapMirrorRepeat(): Unit = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.glCore.GLTexture")
object GLTexture extends js.Object {
  def fromSource(gl: WebGLRenderingContext, source: HTMLImageElement | ImageData | HTMLVideoElement | HTMLCanvasElement, premultipleAlpha: Boolean = ???): GLTexture = js.native
  def fromData(gl: WebGLRenderingContext, data: js.Array[Double], width: Double, height: Double): GLTexture = js.native
}

@js.native
trait Attrib extends js.Object {
  var attribute: js.Any = js.native
  var normalized: Boolean = js.native
  var stride: Double = js.native
  var start: Double = js.native
  var buffer: ArrayBuffer = js.native
}

@js.native
trait WebGLRenderingContextAttribute extends js.Object {
  var buffer: WebGLBuffer = js.native
  var attribute: js.Any = js.native
  var `type`: Double = js.native
  var normalized: Boolean = js.native
  var stride: Double = js.native
  var start: Double = js.native
}

@js.native
trait AttribState extends js.Object {
  var tempAttribState: js.Array[Attrib] = js.native
  var attribState: js.Array[Attrib] = js.native
}

@js.native
@JSGlobal("PIXI.glCore.VertexArrayObject")
class VertexArrayObject protected () extends js.Object {
  def this(gl: WebGLRenderingContext, state: WebGLState) = this()
  var gl: WebGLRenderingContext = js.native
  var attributes: js.Array[Attrib] = js.native
  var indexBuffer: GLBuffer = js.native
  var dirty: Boolean = js.native
  def bind(): VertexArrayObject = js.native
  def unbind(): VertexArrayObject = js.native
  def activate(): VertexArrayObject = js.native
  def addAttribute(buffer: GLBuffer, attribute: Attrib, `type`: Double, normalized: Boolean, stride: Double, start: Double): VertexArrayObject = js.native
  def addIndex(buffer: GLBuffer, options: js.Any = ???): VertexArrayObject = js.native
  def clear(): VertexArrayObject = js.native
  def draw(`type`: Double, size: Double, start: Double): VertexArrayObject = js.native
  def destroy(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.glCore.VertexArrayObject")
object VertexArrayObject extends js.Object {
  var FORCE_NATIVE: Boolean = js.native
}

@JSGlobal("PIXI.glCore")
@js.native
object GlCore extends js.Object {
  def createContext(view: HTMLCanvasElement, options: ContextOptions = ???): WebGLRenderingContext = js.native
  def setVertexAttribArrays(gl: WebGLRenderingContext, attribs: js.Array[Attrib], state: WebGLState = ???): WebGLRenderingContext | Unit = js.native
}

}

@js.native
trait DecomposedDataUri extends js.Object {
  var mediaType: String = js.native
  var subType: String = js.native
  var encoding: String = js.native
  var data: js.Any = js.native
}

package utils {

  package isMobile {

    @js.native
    @JSGlobal("PIXI.utils.isMobile.apple")
    object apple extends js.Object {
      val phone: Boolean = js.native
      val ipod: Boolean = js.native
      val tablet: Boolean = js.native
      val device: Boolean = js.native
    }

    @js.native
    @JSGlobal("PIXI.utils.isMobile.android")
    object android extends js.Object {
      val phone: Boolean = js.native
      val tablet: Boolean = js.native
      val device: Boolean = js.native
    }

    @js.native
    @JSGlobal("PIXI.utils.isMobile.amazon")
    object amazon extends js.Object {
      val phone: Boolean = js.native
      val tablet: Boolean = js.native
      val device: Boolean = js.native
    }

    @js.native
    @JSGlobal("PIXI.utils.isMobile.windows")
    object windows extends js.Object {
      val phone: Boolean = js.native
      val tablet: Boolean = js.native
      val device: Boolean = js.native
    }

    @js.native
    @JSGlobal("PIXI.utils.isMobile.other")
    object other extends js.Object {
      val blackberry10: Boolean = js.native
      val blackberry: Boolean = js.native
      val opera: Boolean = js.native
      val firefox: Boolean = js.native
      val chrome: Boolean = js.native
      val device: Boolean = js.native
    }

    @JSGlobal("PIXI.utils.isMobile")
    @js.native
    object IsMobile extends js.Object {
      val seven_inch: Boolean = js.native
      val any: Boolean = js.native
      val phone: Boolean = js.native
      val tablet: Boolean = js.native
    }

  }

  @js.native
  @JSGlobal("PIXI.utils.EventEmitter")
  class EventEmitter extends js.Object {
    def eventNames(): js.Array[String | js.Symbol] = js.native

    def listeners(event: String | js.Symbol): js.Array[js.Function] = js.native

    def listeners(event: String | js.Symbol, exists: Boolean): Boolean = js.native

    def emit(event: String | js.Symbol, args: js.Any*): Boolean = js.native

    def on(event: String | js.Symbol, fn: js.Function, context: js.Any = ???): this.type = js.native

    def once(event: String | js.Symbol, fn: js.Function, context: js.Any = ???): this.type = js.native

    def removeListener(event: String | js.Symbol, fn: js.Function = ???, context: js.Any = ???, once: Boolean = ???): this.type = js.native

    def removeAllListeners(event: String | js.Symbol = ???): this.type = js.native

    def off(event: String | js.Symbol, fn: js.Function = ???, context: js.Any = ???, once: Boolean = ???): this.type = js.native

    def addListener(event: String | js.Symbol, fn: js.Function, context: js.Any = ???): this.type = js.native

    def setMaxListeners(): this.type = js.native
  }

  @js.native
  @JSGlobal("PIXI.utils.EventEmitter")
  object EventEmitter extends js.Object {
    var prefixed: String | Boolean = js.native
    var EventEmitter: js.Any = js.native
  }

  @JSGlobal("PIXI.utils")
  @js.native
  object Utils extends js.Object {
    def uid(): Double = js.native

    def hex2rgb(hex: Double, out: js.Array[Double] = ???): js.Array[Double] = js.native

    def hex2string(hex: Double): String = js.native

    def rgb2hex(rgb: js.Array[Double]): Double = js.native

    def canUseNewCanvasBlendModes(): Boolean = js.native

    def getResolutionOfUrl(url: String, defaultValue: Double = ???): Double = js.native

    def getSvgSize(svgString: String): js.Dynamic = js.native

    def decomposeDataUri(dataUri: String): DecomposedDataUri | Unit = js.native

    def getUrlFileExtension(url: String): String | Unit = js.native

    def sayHello(`type`: String): Unit = js.native

    def skipHello(): Unit = js.native

    def isWebGLSupported(): Boolean = js.native

    def sign(n: Double): Double = js.native

    def removeItems[T](arr: js.Array[T], startIdx: Double, removeCount: Double): Unit = js.native

    val TextureCache: js.Any = js.native
    val BaseTextureCache: js.Any = js.native
  }

}

package core {

  @JSGlobal("PIXI.core")
  @js.native
  object Core extends js.Object {
    type SpriteBatch = ParticleContainer
    type AssetLoader = loaders.Loader
    type Stage = Container
    type DisplayObjectContainer = Container
    type Strip = mesh.Mesh
    type Rope = mesh.Rope
    type ParticleContainer = particles.ParticleContainer
    type MovieClip = extras.AnimatedSprite
    type TilingSprite = extras.TilingSprite
    type BaseTextureCache = js.Any
    type BitmapText = extras.BitmapText
    type math = js.Any
    type AbstractFilter = Filter
    type TransformManual = TransformBase
    type TARGET_FPMS = Double
    type FILTER_RESOLUTION = Double
    type RESOLUTION = Double
    type MIPMAP_TEXTURES = js.Any
    type SPRITE_BATCH_SIZE = Double
    type SPRITE_MAX_TEXTURES = Double
    type RETINA_PREFIX = RegExp | String
    type DEFAULT_RENDER_OPTIONS = Double
    type PRECISION = String
  }

}