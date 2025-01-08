import cats.effect._
import cats.syntax.all._

import scala.util.Random
import scala.concurrent.duration._
import java.util.Timer
import java.util.TimerTask

// Player representation
case class Player(var x: Int, var y: Int)

// Monster representation
case class Monster(var x: Int, var y: Int)

// Bomb representation with turns until explosion
case class Bomb(var x: Int, var y: Int, var turnsUntilExplosion: Int, var exploded: Boolean = false, var explosionTurn: Option[Int] = None)

// GameState representation with added monsters, game over flag, and player move flag
case class GameState(player: Player, monsters: List[Monster], grid: Array[Array[Char]], bombs: List[Bomb], turns: Int = 0, gameOver: Boolean = false, firstMove: Boolean = false, explosionClearTurn: Option[Int] = None)

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
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#' ,'#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.' , '.', '.', '.', '.', '.', '.', '.', '.','.', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.' , '.', '.', '.', '.', '.', '.', '.', '#','.', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.' , '.', '.', '.', '.', '.', '.', '.', '#','.', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.' , '.', '.', '.', '.', '.', '.', '.', '#','.', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.' , '.', '.', '.', '.', '.', '.', '.', '#','.', '#'),
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#' ,'#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
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
        } else if (state.monsters.exists(m => m.x == i && m.y == j)) {
          // If it's a monster's position, mark it as '2'
          print(" 2 ")
        } else {
          print(s" ${state.grid(i)(j)} ")
        }
      }
      println()
    }
  }

  // Handle player movement
  def movePlayer(state: GameState, direction: Char): GameState = {
    directions.get(direction.toLower) match {  // Convert to lowercase before lookup
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
    state.copy(firstMove = true) // Mark that the player has moved
  }

  // Place a bomb at the player's position (side-effectful) 
  def placeBomb(state: GameState): IO[GameState] = IO {
    val newBomb = Bomb(state.player.x, state.player.y, turnsUntilExplosion = 4)  // Set explosion delay to 4 turns
    val updatedBombs = newBomb :: state.bombs // Add bomb to the list of bombs

    // Place the bomb at the player's current position
    state.grid(state.player.x)(state.player.y) = 'B'

    println(s"Bomb placed at (${state.player.x}, ${state.player.y})")

    // Return the updated state with bombs
    state.copy(bombs = updatedBombs)
  }

  // Explosion logic (with radius of 2 on both X and Y axes)
  def explodeBomb(bomb: Bomb, state: GameState): GameState = {
    if (!bomb.exploded) {
      // Mark the bomb location with 'X' to represent explosion
      markExplosion(bomb.x, bomb.y, state)

      // Mark adjacent cells in the explosion radius (up, down, left, right)
      for (i <- -2 to 2) {
        markExplosion(bomb.x + i, bomb.y, state)  // Left and right
        markExplosion(bomb.x, bomb.y + i, state)  // Up and down
      }

      bomb.exploded = true // Mark as exploded
      bomb.explosionTurn = Some(state.turns) // Track the turn the explosion happens
      println(s"Bomb exploded at (${bomb.x}, ${bomb.y})")

      // Remove the bomb from the list of bombs
      val updatedBombs = state.bombs.filterNot(b => b == bomb)

      // Check if the player is in the explosion radius
      if (isPlayerInExplosion(bomb.x, bomb.y, state)) {
        println("Player caught in the explosion! Game Over!")
        return state.copy(gameOver = true) // Set the game to over and return the updated state
      }

      // Remove monsters that are within the explosion radius
      val monstersAfterExplosion = state.monsters.filterNot(m =>
        isInExplosionRadius(m.x, m.y, bomb.x, bomb.y)
      )

      // Return the updated state with the bomb removed and the monsters removed
      return state.copy(
        monsters = monstersAfterExplosion,
        bombs = updatedBombs,  // Remove the exploded bomb
        explosionClearTurn = Some(state.turns + 1) // Set the turn when to clear the explosion
      )
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

    // Check if player is within the explosion radius (2 blocks in all directions)
    (playerX == bombX && playerY == bombY) ||
      (playerX == bombX - 2 && playerY == bombY) || // Up 2
      (playerX == bombX + 2 && playerY == bombY) || // Down 2
      (playerX == bombX && playerY == bombY - 2) || // Left 2
      (playerX == bombX && playerY == bombY + 2) || // Right 2
      (playerX == bombX - 1 && playerY == bombY) || // Up 1
      (playerX == bombX + 1 && playerY == bombY) || // Down 1
      (playerX == bombX && playerY == bombY - 1) || // Left 1
      (playerX == bombX && playerY == bombY + 1)     // Right 1
  }

 
  // Check if any bombs should explode based on turns
  def checkBombs(state: GameState): GameState = {
    // Check for bombs that should explode or clear
    val stateAfterBombs = state.bombs.foldLeft(state) { (updatedState, bomb) =>
      // Decrement the turns until explosion
      if (bomb.turnsUntilExplosion > 0) {
        bomb.turnsUntilExplosion -= 1
      }
      // If it's time for the bomb to explode
      if (bomb.turnsUntilExplosion == 0 && !bomb.exploded) {
        println(s"Bomb at (${bomb.x}, ${bomb.y}) exploded!")
        explodeBomb(bomb, updatedState) // Update the state after explosion
      } else {
        updatedState
      }
    }

    // If it's time to clear the explosion
    stateAfterBombs.explosionClearTurn match {
      case Some(clearTurn) if stateAfterBombs.turns >= clearTurn =>
        // Clear the explosion marks (replace 'X' with '.')
        val newGrid = stateAfterBombs.grid.map(_.map {
          case 'X' => '.' // Replace 'X' with '.'
          case other => other
        })
        stateAfterBombs.copy(grid = newGrid, explosionClearTurn = None) // Reset the clear turn
      case _ => stateAfterBombs
    }
  }


  // Check if the monster is within the explosion radius
  def isInExplosionRadius(monsterX: Int, monsterY: Int, bombX: Int, bombY: Int): Boolean = {
    (monsterX == bombX && monsterY == bombY) ||
      (monsterX == bombX - 2 && monsterY == bombY) || // Up 2
      (monsterX == bombX + 2 && monsterY == bombY) || // Down 2
      (monsterX == bombX && monsterY == bombY - 2) || // Left 2
      (monsterX == bombX && monsterY == bombY + 2) || // Right 2
      (monsterX == bombX - 1 && monsterY == bombY) || // Up 1
      (monsterX == bombX + 1 && monsterY == bombY) || // Down 1
      (monsterX == bombX && monsterY == bombY - 1) || // Left 1
      (monsterX == bombX && monsterY == bombY + 1)     // Right 1
  }

  // Move monsters randomly after the first move
  def moveMonsters(state: GameState): GameState = {
    if (!state.firstMove) return state // Don't move monsters until the player moves at least once

    val random = new Random()

    // Update each monster's position
    val updatedMonsters = state.monsters.map { monster =>
      val possibleMoves = directions.values.filter { case (dx, dy) =>
        val newX = monster.x + dx
        val newY = monster.y + dy
        newX >= 0 && newX < state.grid.length && newY >= 0 && newY < state.grid(newX).length &&
          state.grid(newX)(newY) == '.' // Ensure it's an empty space
      }

      if (possibleMoves.nonEmpty) {
        // Choose a random move from the available options
        val (dx, dy) = possibleMoves.toSeq(random.nextInt(possibleMoves.size))
        monster.x += dx
        monster.y += dy
      }
      monster
    }

    state.copy(monsters = updatedMonsters)
  }

  // Check if the player is within one block of any monster (adjacent)
  def isPlayerNearMonster(playerX: Int, playerY: Int, monsters: List[Monster]): Boolean = {
    monsters.exists { monster =>
      // Check if the player is one block away from the monster (up, down, left, right)
      (playerX == monster.x && (playerY == monster.y - 1 || playerY == monster.y + 1)) || // Vertically adjacent
        (playerY == monster.y && (playerX == monster.x - 1 || playerX == monster.x + 1))   // Horizontally adjacent
    }
  }

  // Modify the game loop to check if the player is adjacent to a monster **after displaying the grid**
  def gameLoop(state: GameState): IO[Unit] = {
    def loop(state: GameState): IO[Unit] = {
      if (state.gameOver) {
        IO(println("Game Over!"))
      } else {
        // First, display the grid and increment the turn
        val newState = checkBombs(state) // Check for bombs that should explode
        val afterMonsterMoveState = moveMonsters(newState) // Move monsters randomly if player has moved

        // Display the grid after monster move and increment the turn
        displayGrid(afterMonsterMoveState) *> IO {
          println(s"Turn: ${afterMonsterMoveState.turns + 1}")
        } *> IO {
          val updatedState = afterMonsterMoveState.copy(turns = afterMonsterMoveState.turns + 1)
          // Check if the player is adjacent to any monster after displaying the grid
          if (isPlayerNearMonster(updatedState.player.x, updatedState.player.y, updatedState.monsters)) {
            // If the player is adjacent to a monster, end the game
            println("You are too close to a monster!")
            updatedState.copy(gameOver = true)
          } else {
            updatedState
          }
        } flatMap { finalState =>
          if (finalState.gameOver) {
            IO(println("Game Over!"))
          } else {
            // Allow the player to make a move or place a bomb
            IO(println("Enter move (w/a/s/d to move, E to place bomb):")) *> IO.readLine.flatMap {
              case input if input.length == 1 && directions.contains(input.head) =>
                // Move the player
                val updatedState = movePlayer(finalState, input.head)
                val newTurnState = updatedState.copy(turns = updatedState.turns + 1) // Increment turn
                loop(newTurnState) // Continue to next turn

              case "E" | "e" =>
                placeBomb(finalState).flatMap { updatedState =>
                  val newTurnState = updatedState.copy(turns = updatedState.turns + 1) // Increment turn
                  loop(newTurnState) // Continue to next turn after placing bomb
                }

              case _ =>
                IO {
                  println("Invalid command!")
                  loop(finalState)
                }
            }
          }
        }
      }
    }
    loop(state) // Start the game loop
  }

  // Game state initializer with monsters
  def initialState: GameState = {
    val monsters = List(Monster(3, 2), Monster(4, 2)) // Two monsters at initial positions
    GameState(Player(1, 1), monsters, initialGrid, List(), turns = 0)
  }

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
