package Server

import java.util.UUID

// GameSession and GameSession storage
case class GameSession(id: UUID, name: String, gameRoomName: String, players: List[String], grid: Vector[Vector[String]])

object GameSession {
  def createSession(gameRoomName: String): GameSession = {
    val id = UUID.randomUUID()
    val initialGrid = new GameState().initialGrid  // Assuming you want to send the initial grid
    GameSession(id, "Bomberman", gameRoomName, List(), initialGrid)
  }
}
