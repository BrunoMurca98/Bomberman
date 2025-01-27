package Server

import cats.effect.IO
import fs2.concurrent.Topic
import org.http4s.websocket.WebSocketFrame

// GameSession and GameSession storage
case class GameSession(
    state: GameState,
    topic: Topic[IO, WebSocketFrame.Text]
)

object GameSession {
  // apply - GameSession
  // of - IO[GameSession]
  // make - Resource[IO, GameSession]

  def of(name: String): IO[GameSession] =
    for {
      id    <- IO.randomUUID
      topic <- Topic[IO, WebSocketFrame.Text]
    } yield GameSession(
      state = GameState(
        id = id,
        name = name,
        players = List.empty,
        grid = GameState.initialGrid
      ),
      topic = topic
    )
}
