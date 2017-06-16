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
case class Spaceship(common : CommonState) extends Message 
case class Laser(common : CommonState, shooter : Int) extends Message
case class Cmd(sId : Int, cmd : Command) extends Message
case class ClientId(clientId : Int) extends Message
case class Destroy(destroy : Int) extends Message
case class SetLifes(sId : Int, lifes : Int) extends Message

case class CommonState(ident : Int, pos : (Double, Double), velo : (Double, Double), acc : Double, omega : Double, phi : Double)  

sealed trait AColor 
case object Brown extends AColor
case object Gray extends AColor

sealed trait ASize
case object Tiny extends ASize
case object Small extends ASize
case object Medium extends ASize
case object Big extends ASize

sealed trait Command 
case object F extends Command
case object B extends Command
case object L extends Command
case object R extends Command
case object S extends Command

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

object Command extends JSONConfig {
  implicit val encodeMessage: Encoder[Command] =
    deriveEncoder[Command]

  implicit val decodeMessage: Decoder[Command] =
    deriveDecoder[Command]
}
