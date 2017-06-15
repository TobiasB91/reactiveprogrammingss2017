import scala.util.Random

/**
  * Created by martin on 23.05.17.
  */
case class Vector2d(x: Double, y: Double) {
  def * (scalar: Double) = Vector2d(x * scalar, y * scalar)
  def / (scalar: Double) = this * (1 / scalar)
  def + (that: Vector2d)   = Vector2d(this.x + that.x, this.y + that.y)
  def - (that: Vector2d)   = Vector2d(this.x - that.x, this.y - that.y)
  def rotate (rad: Double) = Vector2d(
    x * Math.cos(rad) - y * Math.sin(rad),
    x * Math.sin(rad) + y * Math.cos(rad))
  def magnitude: Double = Math.sqrt(Math.pow(x,2)+Math.pow(y,2))
}

object Vector2d {
  val zero = Vector2d(0,0)
  val unit = Vector2d(0,1)
  def random(scale: Double = 1.0) =
    Vector2d(
      Random.nextDouble() * 2 * scale - scale,
      Random.nextDouble() * 2 * scale - scale)
}