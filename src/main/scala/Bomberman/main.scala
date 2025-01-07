import cats.effect._
import cats.syntax.all._

import scala.concurrent.duration._
import java.util.Timer
import java.util.TimerTask

// Player representation
case class Player(var x: Int, var y: Int)

// Bomb representation with turns until explosion
case class Bomb(var x: Int, var y: Int, var turnsUntilExplosion: Int, var exploded: Boolean = false, var explosionTurn: Option[Int] = None)

// GameState representation with added `turns` field and game over flag
case class GameState(player: Player, grid: Array[Array[Char]], bombs: List[Bomb], turns: Int = 0, gameOver: Boolean = false, explosionClearTurn: Option[Int] = None)

// Commands to modify the state
sealed trait Command
case class Move(direction: Char) extends Command
case object PlaceBomb extends Command

object Bomberman {

  // Directions for movement
  val directions = Map(
    'w' -> (-1, 0), // Up
    's' -> (1, 0),  // Down
    'a' -> (0, -1), // Left
    'd' -> (0, 1)   // Right
  )

  // The game map, represented as a 2D array of chars
  val initialGrid: Array[Array[Char]] = Array(
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '#', '#', '#', '.', '#', '#', '.', '#', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '#'),
    Array('#', '.', '#', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#')
  )

  // Display the grid (side-effectful)
  def displayGrid(state: GameState): IO[Unit] = IO {
    println(s"Turn: ${state.turns}")
    println("Bomberman Game:")
    for (i <- state.grid.indices) {
      for (j <- state.grid(i).indices) {
        // If it's the player's position, mark it as '1'
        if (i == state.player.x && j == state.player.y) {
          print(" 1 ")
        } else {
          print(s" ${state.grid(i)(j)} ")
        }
      }
      println()
    }
  }

  // Handle player movement
  def movePlayer(state: GameState, direction: Char): GameState = {
    directions.get(direction) match {
      case Some((dx, dy)) =>
        val newX = state.player.x + dx
        val newY = state.player.y + dy
        if (newX >= 0 && newX < state.grid.length && newY >= 0 && newY < state.grid(newX).length && state.grid(newX)(newY) != '#') {
          // Update the player's position
          state.grid(state.player.x)(state.player.y) = '.'
          state.player.x = newX
          state.player.y = newY
          state.grid(state.player.x)(state.player.y) = '1'
        }
      case None => println("Invalid direction!")
    }
    state
  }

  // Place a bomb at the player's position (side-effectful)
  def placeBomb(state: GameState): IO[GameState] = IO {
    val newBomb = Bomb(state.player.x, state.player.y, turnsUntilExplosion = 3)  // Set explosion delay to 3 turns
    val updatedBombs = newBomb :: state.bombs // Add bomb to the list of bombs

    // Place the bomb at the player's current position
    state.grid(state.player.x)(state.player.y) = 'B'

    println(s"Bomb placed at (${state.player.x}, ${state.player.y})")

    // Return the updated state with bombs
    state.copy(bombs = updatedBombs)
  }

  // Explosion logic (with radius of 1 on both X and Y axes)
  def explodeBomb(bomb: Bomb, state: GameState): GameState = {
    if (!bomb.exploded) {
      // Mark the bomb location with 'X' to represent explosion
      markExplosion(bomb.x, bomb.y, state)

      // Mark adjacent cells in the explosion radius (up, down, left, right)
      markExplosion(bomb.x - 1, bomb.y, state) // Up
      markExplosion(bomb.x + 1, bomb.y, state) // Down
      markExplosion(bomb.x, bomb.y - 1, state) // Left
      markExplosion(bomb.x, bomb.y + 1, state) // Right

      bomb.exploded = true // Mark as exploded
      bomb.explosionTurn = Some(state.turns) // Track the turn the explosion happens
      println(s"Bomb exploded at (${bomb.x}, ${bomb.y})")

      // Check if the player is in the explosion radius
      if (isPlayerInExplosion(bomb.x, bomb.y, state)) {
        println("Player caught in the explosion! Game Over!")
        return state.copy(gameOver = true) // Set the game to over and return the updated state
      }
    }
    state
  }

  // Mark a cell as exploded if within bounds
  def markExplosion(x: Int, y: Int, state: GameState): Unit = {
    if (x >= 0 && x < state.grid.length && y >= 0 && y < state.grid(x).length && state.grid(x)(y) != '#') {
      state.grid(x)(y) = 'X' // Mark the cell as exploded
    }
  }

  // Check if the player is caught in the explosion radius
  def isPlayerInExplosion(bombX: Int, bombY: Int, state: GameState): Boolean = {
    val playerX = state.player.x
    val playerY = state.player.y

    // Check if player is within the explosion radius
    (playerX == bombX && playerY == bombY) ||
      (playerX == bombX - 1 && playerY == bombY) || // Up
      (playerX == bombX + 1 && playerY == bombY) || // Down
      (playerX == bombX && playerY == bombY - 1) || // Left
      (playerX == bombX && playerY == bombY + 1)     // Right
  }

  // Check if any bombs should explode based on turns
  def checkBombs(state: GameState): GameState = {
    state.bombs.filterNot(_.exploded).foldLeft(state) { (updatedState, bomb) =>
      // Decrement the turns until explosion
      if (bomb.turnsUntilExplosion > 0) {
        bomb.turnsUntilExplosion -= 1
      }
      // If it's time for the bomb to explode
      if (bomb.turnsUntilExplosion == 0) {
        println(s"Bomb at (${bomb.x}, ${bomb.y}) exploded!")
        explodeBomb(bomb, updatedState) // Update the state after explosion
      } else {
        updatedState
      }
    }
  }

  // Clear explosion marks ('X') after a certain number of turns
  def clearExplosionMarks(state: GameState): GameState = {
    val turnToClear = state.turns - 1 // The turn after the explosion
    val updatedGrid = state.grid.map(_.clone()) // Copy the grid to modify it

    // Clear the 'X' marks if it's time to clear them
    state.bombs.filter(bomb => bomb.explosionTurn.contains(turnToClear)).foreach { bomb =>
      // Clear explosion marks around the bomb
      clearExplosion(bomb.x, bomb.y, updatedGrid)
      clearExplosion(bomb.x - 1, bomb.y, updatedGrid) // Up
      clearExplosion(bomb.x + 1, bomb.y, updatedGrid) // Down
      clearExplosion(bomb.x, bomb.y - 1, updatedGrid) // Left
      clearExplosion(bomb.x, bomb.y + 1, updatedGrid) // Right
    }

    state.copy(grid = updatedGrid) // Return the updated state
  }

  // Clear a single explosion mark if within bounds
  def clearExplosion(x: Int, y: Int, grid: Array[Array[Char]]): Unit = {
    if (x >= 0 && x < grid.length && y >= 0 && y < grid(x).length && grid(x)(y) == 'X') {
      grid(x)(y) = '.' // Clear the 'X' mark
    }
  }

  // Main game loop (side-effectful)
  def gameLoop(state: GameState): IO[Unit] = {
    def loop(state: GameState): IO[Unit] = {
      if (state.gameOver) {
        IO(println("Game Over! You have died."))
      } else {
        val newState = checkBombs(state) // Check for bombs that should explode
        val finalState = clearExplosionMarks(newState) // Clear explosion marks after a turn

        displayGrid(finalState) *> IO {
          println("Enter move (w/a/s/d to move, E to place bomb):")
        } *> IO.readLine.flatMap {
          case input if input.length == 1 && directions.contains(input.head) =>
            val updatedState = movePlayer(finalState, input.head) // Move the player
            val nextState = updatedState.copy(turns = updatedState.turns + 1) // Increment turn count
            loop(nextState) // Continue to next turn

          case "E" | "e" =>
            placeBomb(finalState).flatMap { updatedState =>
              val nextState = updatedState.copy(turns = updatedState.turns + 1) // Increment turn count after placing bomb
              loop(nextState)
            }

          case _ =>
            IO {
              println("Invalid command!")
              loop(finalState)
            }
        }
      }
    }
    loop(state) // Start the game loop
  }


  // Game state initializer
  def initialState: GameState = GameState(Player(1, 1), initialGrid, List(), turns = 0)

  // Start the game
  def startGame(): IO[Unit] = {
    gameLoop(initialState)
  }
}

// The main application entry point
object BombermanApp extends IOApp.Simple {
  def run: IO[Unit] = {
    Bomberman.startGame()
  }
}
