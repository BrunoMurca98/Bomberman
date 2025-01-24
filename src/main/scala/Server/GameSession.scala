package Server

import java.util.UUID

// GameSession and GameSession storage
case class GameSession(id: UUID, name: String, players: List[String], grid: Vector[Vector[String]])

object GameSession {
  def createSession(name: String): GameSession = {
    val id = UUID.randomUUID()
    val initialGrid = new GameState().initialGrid  // Assuming you want to send the initial grid
    GameSession(id, name, List(), initialGrid)
  }

  def addPlayerToGrid(grid: Vector[Vector[String]], playerId: String): Vector[Vector[String]] = {
    // Place the player at (1, 1)
    grid.updated(1, grid(1).updated(1, "P"))
  }

  def movePlayer(grid: Vector[Vector[String]], direction: String): Vector[Vector[String]] = {
    // Find the player's current position (marked as "P")
    val playerPos = findPlayerPosition(grid)
    val newPos = direction match {
      case "up" => (playerPos._1 - 1, playerPos._2)
      case "down" => (playerPos._1 + 1, playerPos._2)
      case "left" => (playerPos._1, playerPos._2 - 1)
      case "right" => (playerPos._1, playerPos._2 + 1)
      case _ => playerPos
    }

    // Ensure the new position is valid and not a wall
    if (isValidMove(newPos, grid)) {
      updateGrid(grid, playerPos, newPos)
    } else {
      grid // No change if the move is invalid
    }
  }

  // Helper to find the player's position (marked as "P")
  def findPlayerPosition(grid: Vector[Vector[String]]): (Int, Int) = {
    grid.zipWithIndex.collectFirst {
      case (row, i) => row.zipWithIndex.collectFirst {
        case (cell, j) if cell == "P" => (i, j)
      }
    }.flatten.getOrElse((0, 0)) // Default to (0, 0) if the player is not found
  }

  // Helper to check if the new position is valid
  def isValidMove(pos: (Int, Int), grid: Vector[Vector[String]]): Boolean = {
    val (x, y) = pos
    if (x >= 0 && x < grid.length && y >= 0 && y < grid(x).length) {
      grid(x)(y) != "#" // Valid if the position is not a wall
    } else {
      false // Out of bounds
    }
  }

  // Helper to update the grid with the new player position
  def updateGrid(grid: Vector[Vector[String]], oldPos: (Int, Int), newPos: (Int, Int)): Vector[Vector[String]] = {
    grid.updated(oldPos._1, grid(oldPos._1).updated(oldPos._2, "."))
      .updated(newPos._1, grid(newPos._1).updated(newPos._2, "P"))
  }

}
