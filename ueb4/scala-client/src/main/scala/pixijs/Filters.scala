package pixijs.filters

import scala.scalajs.js
import js.annotation._
import js.|
import pixijs._

@js.native
@JSGlobal("PIXI.filters.AsciiFilter")
class AsciiFilter extends Filter {
  var size: Double = js.native
}

@js.native
@JSGlobal("PIXI.filters.BloomFilter")
class BloomFilter extends Filter {
  var blur: Double = js.native
  var blurX: Double = js.native
  var blurY: Double = js.native
}

@js.native
@JSGlobal("PIXI.filters.ConvolutionFilter")
class ConvolutionFilter protected () extends Filter {
  def this(matrix: js.Array[Double], width: Double, height: Double) = this()
  var height: Double = js.native
  var width: Double = js.native
  var matrix: js.Array[Double] = js.native
}

@js.native
@JSGlobal("PIXI.filters.CrossHatchFilter")
class CrossHatchFilter extends Filter {
}

@js.native
@JSGlobal("PIXI.filters.DotFilter")
class DotFilter extends Filter {
  var angle: Double = js.native
  var scale: Double = js.native
}

@js.native
@JSGlobal("PIXI.filters.EmbossFilter")
class EmbossFilter extends Filter {
  var strength: Double = js.native
}

@js.native
@JSGlobal("PIXI.filters.PixelateFilter")
class PixelateFilter extends Filter {
  var size: Point = js.native
}

@js.native
@JSGlobal("PIXI.filters.RGBSplitFilter")
class RGBSplitFilter extends Filter {
  var blue: Point = js.native
  var green: Point = js.native
  var red: Point = js.native
}

@js.native
@JSGlobal("PIXI.filters.ShockwaveFilter")
class ShockwaveFilter extends Filter {
  var center: js.Dictionary[Double] = js.native
  var params: js.Dictionary[Double] = js.native
  var time: Double = js.native
}

@js.native
@JSGlobal("PIXI.filters.TiltShiftAxisFilter")
class TiltShiftAxisFilter extends Filter {
  var blur: Double = js.native
  var end: Point = js.native
  var gradientBlur: Double = js.native
  var start: Point = js.native
  def updateDelta(): Unit = js.native
}

@js.native
@JSGlobal("PIXI.filters.TiltShiftFilter")
class TiltShiftFilter extends Filter {
  var tiltShiftXFilter: TiltShiftXFilter = js.native
  var tiltShiftYFilter: TiltShiftYFilter = js.native
  var blur: Double = js.native
  var end: Point = js.native
  var gradientBlur: Double = js.native
  var start: Point = js.native
}

@js.native
@JSGlobal("PIXI.filters.TiltShiftYFilter")
class TiltShiftYFilter extends TiltShiftAxisFilter {
}

@js.native
@JSGlobal("PIXI.filters.TiltShiftXFilter")
class TiltShiftXFilter extends TiltShiftAxisFilter {
}

@js.native
@JSGlobal("PIXI.filters.TwistFilter")
class TwistFilter extends Filter {
  var angle: Double = js.native
  var offset: Point = js.native
  var radius: Double = js.native
}

