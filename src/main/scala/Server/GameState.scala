package Server

import java.util.UUID

final case class GameState(
    id: UUID,
    name: String,
    players: List[String],
    grid: Vector[Vector[String]]
) {
  // Helper to find the player's position (marked as "P")
  def findPlayerPosition(): (Int, Int) = {

    // TODO: this will not work with different players - please use IDs here to identify
    grid.flatten.zipWithIndex
      .collectFirst { case ("P", i) =>
        (i / grid(0).length, i % grid(0).length)
      }
      .getOrElse((1, 1))
  }

  // TODO: Use playerId in future
  def addPlayer(playerId: String): GameState = {
    // Place the player at (1, 1)
    val updatedGrid = grid.updated(1, grid(1).updated(1, "P"))
    copy(
      grid = updatedGrid,
      players = players :+ playerId
    )
  }

  // Helper to check if the new position is valid
  def isValidMove(pos: (Int, Int)): Boolean = {
    val (x, y) = pos
    if (x >= 0 && x < grid.length && y >= 0 && y < grid(x).length) {
      grid(x)(y) != "#" // Valid if the position is not a wall
    } else {
      false // Out of bounds
    }
  }

  def movePlayer(direction: String): GameState = {
    // Find the player's current position (marked as "P")
    val playerPos = findPlayerPosition()
    val newPos = direction match {
      case "up"    => (playerPos._1 - 1, playerPos._2)
      case "down"  => (playerPos._1 + 1, playerPos._2)
      case "left"  => (playerPos._1, playerPos._2 - 1)
      case "right" => (playerPos._1, playerPos._2 + 1)
      case _       => playerPos
    }

    // Ensure the new position is valid and not a wall
    val updatedGrid = if (isValidMove(newPos)) {
      updateGrid(playerPos, newPos)
    } else {
      grid // No change if the move is invalid
    }

    println(s"$playerPos -> $newPos")

    copy(
      grid = updatedGrid
    )
  }

  // Helper to update the grid with the new player position
  def updateGrid(oldPos: (Int, Int), newPos: (Int, Int)): Vector[Vector[String]] = {

    val withNoPlayer = grid.updated(oldPos._1, grid(oldPos._1).updated(oldPos._2, "."))
    withNoPlayer.updated(newPos._1, withNoPlayer(newPos._1).updated(newPos._2, "P"))
  }
}

object GameState {

  val initialGrid: Vector[Vector[String]] = Vector(
    Vector("#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#"),
    Vector("#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#")
  )

}
