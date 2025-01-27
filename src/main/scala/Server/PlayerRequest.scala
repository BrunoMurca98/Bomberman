package Server

import io.circe.Codec
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveConfiguredCodec
import io.circe.generic.semiauto.deriveCodec

// { "type": "PlayerMove", "player": "player1", "direction": "up" }
// { "type": "PlantBomb", ... }
sealed trait PlayerRequest

object PlayerRequest {
  final case class PlayerMove(player: String, direction: String) extends PlayerRequest
  object PlayerMove {
    implicit val codec: Codec[PlayerMove] = deriveCodec
  }

  implicit val codec: Codec[PlayerRequest] = {
    implicit val configuration: Configuration = Configuration.default.withDiscriminator("type")
    deriveConfiguredCodec
  }
}
