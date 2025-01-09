import cats.effect._
import cats.syntax.all._

import scala.util.Random
import scala.concurrent.duration._
import java.util.Timer
import java.util.TimerTask

// Player representation
case class Player(x: Int, y: Int)

// Monster representation
case class Monster(x: Int, y: Int)

// Bomb representation with turns until explosion
case class Bomb(x: Int, y: Int, turnsUntilExplosion: Int, exploded: Boolean = false, explosionTurn: Option[Int] = None)

// GameState representation with added monsters, game over flag, and player move flag
case class GameState(
                      player: Player,
                      monsters: List[Monster],
                      grid: Array[Array[Char]],
                      bombs: List[Bomb],
                      turns: Int = 0,
                      gameOver: Boolean = false,
                      firstMove: Boolean = false,
                      explosionClearTurn: Option[Int] = None,
                      bombPlaced: Boolean = false // Track if a bomb is placed
                    )

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
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.' , '.', '.', '.', '.', '.', '.', '.', '#','#', '#'),
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
    directions.get(direction.toLower) match {
      case Some((dx, dy)) =>
        val newX = state.player.x + dx
        val newY = state.player.y + dy
        if (newX >= 0 && newX < state.grid.length && newY >= 0 && newY < state.grid(newX).length && state.grid(newX)(newY) != '#') {
          // Move the player immutably by creating a new Player
          state.grid(state.player.x)(state.player.y) = '.'  // Clear the old position
          val newPlayer = state.player.copy(x = newX, y = newY)
          state.grid(newX)(newY) = '1'  // Mark the new position

          state.copy(player = newPlayer)  // Update the state with the new player position
        } else {
          state  // Invalid move, return the same state
        }

      case None => println("Invalid direction!"); state
    }
  }


  // Place a bomb at the player's position (side-effectful)
  def placeBomb(state: GameState): IO[GameState] = IO {
    if (state.bombPlaced) {
      println("You can only place one bomb at a time! Wait for the previous one to explode.")
      state // Don't place a new bomb, just return the current state
    } else {
      val newBomb = Bomb(state.player.x, state.player.y, turnsUntilExplosion = 4)  // Set explosion delay to 4 turns
      val updatedBombs = newBomb :: state.bombs // Add bomb to the list of bombs

      // Place the bomb at the player's current position
      state.grid(state.player.x)(state.player.y) = 'B'

      println(s"Bomb placed at (${state.player.x}, ${state.player.y})")

      // Update the state to reflect that a bomb has been placed
      state.copy(bombs = updatedBombs, bombPlaced = true) // Set bombPlaced to true
    }
  }

  // Check if any bombs should explode based on turns
  def checkBombs(state: GameState): GameState = {
    // Only check bombs if the turn has incremented
    if (state.turns > state.turns - 1) {
      // Check for bombs that should explode or clear
      val stateAfterBombs = state.bombs.foldLeft(state) { (updatedState, bomb) =>
        // Decrement the turns until explosion and create a new Bomb instance
        if (bomb.turnsUntilExplosion > 0) {
          val updatedBomb = bomb.copy(turnsUntilExplosion = bomb.turnsUntilExplosion - 1)
          updatedState.copy(bombs = updatedState.bombs.map {
            case b if b == bomb => updatedBomb
            case b => b
          })
        } else if (bomb.turnsUntilExplosion == 0 && !bomb.exploded) {
          println(s"Bomb at (${bomb.x}, ${bomb.y}) exploded!")
          explodeBomb(bomb, updatedState) // Explode the bomb and update the state
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
    } else {
      state // If turn has not incremented, just return the same state
    }
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

      // Create a new Bomb with exploded = true and the current explosion turn
      val explodedBomb = bomb.copy(exploded = true, explosionTurn = Some(state.turns))

      // Remove the bomb from the list of bombs (bomb has exploded)
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

      // Reset the bombPlaced flag after the bomb explodes
      state.copy(
        monsters = monstersAfterExplosion,
        bombs = updatedBombs :+ explodedBomb,  // Add the exploded bomb to the list of bombs
        explosionClearTurn = Some(state.turns + 1), // Set the turn when to clear the explosion
        bombPlaced = false // Allow placing a new bomb after the explosion
      )
    } else {
      state // If the bomb has already exploded, return the state as is
    }
  }



  // Mark a cell as exploded if within bounds and not a wall
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
    // Ensure monsters start moving after the player makes their first move
    if (!state.firstMove) return state // Don't move monsters until the player moves at least once

    val random = new Random()

    // Update each monster's position immutably
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
        // Return a new monster instance with updated position
        monster.copy(x = monster.x + dx, y = monster.y + dy)
      } else {
        monster // If no valid moves, return the monster as is
      }
    }

    // Update the game state with the new list of monsters
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
        // Display the grid and turn number
        displayGrid(state) *> IO {
          println(s"Turn: ${state.turns}")
        } *> IO {
          // Check if the player is adjacent to any monster after displaying the grid
          val updatedState = if (isPlayerNearMonster(state.player.x, state.player.y, state.monsters)) {
            // If the player is adjacent to a monster, end the game
            println("You are too close to a monster!")
            state.copy(gameOver = true)
          } else {
            state
          }

          updatedState
        } flatMap { finalState =>
          if (finalState.gameOver) {
            IO(println("Game Over!"))
          } else {
            // Allow the player to make a move or place a bomb
            IO(println("Enter move (w/a/s/d to move, E to place bomb):")) *> IO.readLine.flatMap {
              case input if input.length == 1 && directions.contains(input.head) =>
                // Handle valid player movement
                val updatedState = movePlayer(finalState, input.head)
                // Set firstMove to true after the player has made their first move
                val stateWithFirstMove = updatedState.copy(firstMove = true)
                // After player moves, increment the turn and move monsters
                val updatedTurnState = stateWithFirstMove.copy(turns = stateWithFirstMove.turns + 1)
                val newStateAfterMonsters = moveMonsters(updatedTurnState) // Move monsters after player move
                // Check bombs after turn increment
                val stateWithBombsChecked = checkBombs(newStateAfterMonsters)
                loop(stateWithBombsChecked) // Continue to next turn after valid input

              case "E" | "e" =>
                // Handle bomb placement
                placeBomb(finalState).flatMap { updatedState =>
                  // Only increment the turn if the bomb was placed successfully
                  if (updatedState != finalState) {
                    // Bomb was placed successfully, so increment turn
                    val updatedTurnState = updatedState.copy(turns = updatedState.turns + 1)
                    val stateWithBombsChecked = checkBombs(updatedTurnState)
                    loop(stateWithBombsChecked) // Continue to next turn after placing bomb
                  } else {
                    // If no bomb was placed (invalid bomb placement), stay at the same turn
                    loop(finalState)
                  }
                }

              case _ =>
                // Handle invalid input: don't move monsters, don't increment turn, just re-prompt
                IO {
                  println("Invalid command! Please enter a valid command.")
                } *> loop(finalState) // Return to the same state, do not change the state
            }
          }
        }
      }
    }

    loop(state) // Start the game loop
  }



  // Game state initializer with monsters
  def initialState: GameState = {
    val monsters = List(Monster(3, 18), Monster(5, 18))  // Two monsters at initial positions
    GameState(Player(5, 16), monsters, initialGrid, List(), turns = 0)
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

