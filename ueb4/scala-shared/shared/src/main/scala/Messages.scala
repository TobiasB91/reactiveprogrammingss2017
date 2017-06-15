import io.circe._
import io.circe.generic.semiauto._

case class TimestampedMessage(
  timestamp: Long,
  payload: Message
)

sealed trait Message
case object Ping extends Message
case object Pong extends Message
case class Asteroid(common : CommonState, size : ASize, color : AColor) extends Message 

case class CommonState(ident : Int, pos : (Double, Double), velo : (Double, Double), acc : Double, omega : Double, phi : Double)  

sealed trait AColor 
case object Brown extends AColor
case object Gray extends AColor

sealed trait ASize
case object Tiny extends ASize
case object Small extends ASize
case object Medium extends ASize
case object Big extends ASize

///////////////////////////
// JSON De/serialization //
///////////////////////////

sealed trait JSONConfig {
  import io.circe.Encoder
  import shapeless.{ Generic, HNil }

  implicit def encodeCaseObject[A <: Product](implicit
    gen: Generic.Aux[A, HNil]
  ): Encoder[A] = Encoder.instance(_ => Json.arr())
}

object TimestampedMessage extends JSONConfig {
  def apply(payload: Message): TimestampedMessage = TimestampedMessage(System.currentTimeMillis(),payload)

  implicit val encodeTimestampedMessage: Encoder[TimestampedMessage] =
    deriveEncoder[TimestampedMessage]

  implicit val decodeTimestampedMessage: Decoder[TimestampedMessage] =
    deriveDecoder[TimestampedMessage]
}

object Message extends JSONConfig {
  implicit val encodeMessage: Encoder[Message] =
    deriveEncoder[Message]

  implicit val decodeMessage: Decoder[Message] =
    deriveDecoder[Message]
}

object CommonState extends JSONConfig {
  implicit val encodeMessage: Encoder[CommonState] =
    deriveEncoder[CommonState]

  implicit val decodeMessage: Decoder[CommonState] =
    deriveDecoder[CommonState]
}

object AColor extends JSONConfig {
  implicit val encodeMessage: Encoder[AColor] =
    deriveEncoder[AColor]

  implicit val decodeMessage: Decoder[AColor] =
    deriveDecoder[AColor]
}

object ASize extends JSONConfig {
  implicit val encodeMessage: Encoder[ASize] =
    deriveEncoder[ASize]

  implicit val decodeMessage: Decoder[ASize] =
    deriveDecoder[ASize]
}
