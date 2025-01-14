
import cats.effect._
import cats.syntax.all._

import scala.util.Random
import scala.concurrent.duration._
import java.util.Timer
import java.util.TimerTask
import scala.annotation.tailrec

// Player representation
case class Player(x: Int, y: Int)

// Monster representation
case class Monster(x: Int, y: Int)

// Bomb representation with additional turnPlaced field
case class Bomb(x: Int, y: Int, turnsUntilExplosion: Int, exploded: Boolean = false, explosionTurn: Option[Int] = None, turnPlaced: Int)


// GameState representation with added monsters, game over flag, and player move flag
case class GameState(
                      player: Player,
                      monsters: List[Monster],
                      grid: Array[Array[String]],
                      bombs: List[Bomb],
                      turns: Int = 0,
                      gameOver: Boolean = false,
                      firstMove: Boolean = false,
                      explosionClearTurn: Option[Int] = None,
                      bombPlaced: Boolean = false,
                      bombPlacedThisTurn: Boolean = false,
                      bombRadius: Int = 2,  // Default bomb radius is 2
                      frozenTurns: Int = 0   // Tracks the number of turns monsters are frozen
                    )


object Bomberman {

  // Directions for movement
  val directions = Map(
    'w' -> (-1, 0), // Up
    's' -> (1, 0),  // Down
    'a' -> (0, -1), // Left
    'd' -> (0, 1)   // Right
  )

  // The game map, represented as a 2D array of chars
  val initialGrid: Array[Array[String]] = Array(
    Array("#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#"),
    Array("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "F", ".", "T", "F", ".", ".", "#"),
    Array("#", ".", ".", ".", ".", ".", ".", ".", "#", ".", ".", ".", ".", ".", "*", ".", ".", "#", ".", "#"),
    Array("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "*", "*", ".", "*", ".", "#", ".", "#"),
    Array("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#", "#", "#"),
    Array("#", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", ".", "#", ".", "#"),
    Array("#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#", "#"),
  )

  // Display the grid (side-effectful)
  def displayGrid(state: GameState): IO[Unit] = IO {
    println(s"Turn: ${state.turns}")
    println("Bomberman Game:")
    for (i <- state.grid.indices) {
      for (j <- state.grid(i).indices) {
        // Show the bomb on the grid only if it hasn't exploded
        val bombToDisplay = state.bombs.find(bomb => bomb.x == i && bomb.y == j && !bomb.exploded)
        bombToDisplay match {
          case Some(bomb) if state.turns - bomb.turnPlaced >= 2 =>
            // Mark the bomb location with 'B' if it's been 2 turns since placement and it hasn't exploded
            print(" \uD83D\uDCA3")
          case _ =>
            if (i == state.player.x && j == state.player.y) {
              // If it's the player's position, mark it as '1'
              print(" 1 ")
            } else if (state.monsters.exists(m => m.x == i && m.y == j)) {
              // If it's a monster's position, mark it as '2'
              print(" 2 ")
            } else {
              // Otherwise, print the grid's normal content
              print(s" ${state.grid(i)(j)} ")
            }
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

        // Check if the new position is within bounds and not a wall or bomb
        if (newX >= 0 && newX < state.grid.length && newY >= 0 && newY < state.grid(newX).length) {
          state.grid(state.player.x)(state.player.y) = "."  // Clear the old position

          // Check if the new position is a valid move (not a wall, breakable wall, or bomb)
          if (state.grid(newX)(newY) != "#" && state.grid(newX)(newY) != "*" && !state.bombs.exists(b => b.x == newX && b.y == newY && !b.exploded)) {
            val newPlayer = state.player.copy(x = newX, y = newY)

            // If the player lands on a freeze power-up, increase freeze turns
            if (state.grid(newX)(newY) == "T") {
              println("Freeze Power-Up Collected! Monsters are frozen for 2 turns! ❄️")
              state.grid(newX)(newY) = "."  // Remove the power-up from the grid
              val newState = state.copy(player = newPlayer, frozenTurns = 3) // Freeze monsters for 2 turns
              // After moving, check if a monster is adjacent and end the game if so
              if (isPlayerNearMonster(newState.player.x, newState.player.y, newState.monsters)) {
                println("A monster is adjacent to you!")
                return newState.copy(gameOver = true)
              }
              return newState
            }
            // If the player lands on a power-up, increase bomb radius
            else if (state.grid(newX)(newY) == "F") {
              println("Power-Up Collected! Your bomb radius has increased! 💥")
              state.grid(newX)(newY) = "."  // Remove the power-up from the grid
              val newState = state.copy(player = newPlayer, bombRadius = state.bombRadius + 1)
              // After moving, check if a monster is adjacent and end the game if so
              if (isPlayerNearMonster(newState.player.x, newState.player.y, newState.monsters)) {
                println("A monster is adjacent to you!")
                return newState.copy(gameOver = true)
              }
              return newState
            }

            val newState = state.copy(player = newPlayer)

            // After moving, check if a monster is adjacent and end the game if so
            if (isPlayerNearMonster(newState.player.x, newState.player.y, newState.monsters)) {
              println("A monster is adjacent to you!")
              return newState.copy(gameOver = true)
            }

            state.grid(newX)(newY) = "1"  // Mark the new position of the player
            return newState
          } else {
            // If the new position is invalid, print message and repeat turn
            println("⚠️ You can't move through walls or bombs! Try a different direction.")
            state // Return the same state (no change), repeating the turn
          }
        } else {
          // If the new position is outside grid bounds, print message and repeat turn
          println("⚠️ You can't move outside the grid! Try a different direction.")
          state  // Return the same state (no change), repeating the turn
        }

      case None =>
        println("❌ Invalid direction! Try 'w', 'a', 's', or 'd'.")
        state  // Return the same state (no change), repeating the turn
    }
  }





  // Place a bomb at the player's position (side-effectful)
  def placeBomb(state: GameState): IO[GameState] = IO {
    if (state.bombPlaced) {
      println("You can only place one bomb at a time! Wait for the previous one to explode.")
      state // Don't place a new bomb, just return the current state
    } else {
      // Set the bombPlaced flag for the current turn
      val newBomb = Bomb(state.player.x, state.player.y, turnsUntilExplosion = 4, turnPlaced = state.turns)  // Set explosion delay to 4 turns
      val updatedBombs = newBomb :: state.bombs // Add bomb to the list of bombs

      // Update the state to reflect that a bomb has been placed
      state.copy(
        bombs = updatedBombs,
        bombPlaced = true, // Set bombPlaced to true
        bombPlacedThisTurn = true // Mark bomb placed in this turn
      )
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
            case "X" => "." // Replace 'X' with '.'
            case other => other
          })
          stateAfterBombs.copy(grid = newGrid, explosionClearTurn = None) // Reset the clear turn
        case _ => stateAfterBombs
      }
    } else {
      state // If turn has not incremented, just return the same state
    }
  }


  def explodeBomb(bomb: Bomb, state: GameState): GameState = {
    if (!bomb.exploded) {
      // Mark explosion
      markExplosion(bomb.x, bomb.y, state)
      markExplosionInDirection(bomb.x, bomb.y, -1, 0, state, state.bombRadius) // Left
      markExplosionInDirection(bomb.x, bomb.y, 1, 0, state, state.bombRadius)  // Right
      markExplosionInDirection(bomb.x, bomb.y, 0, -1, state, state.bombRadius) // Up
      markExplosionInDirection(bomb.x, bomb.y, 0, 1, state, state.bombRadius)  // Down

      // Debug print to verify player and explosion positions
      println(s"Explosion at: (${bomb.x}, ${bomb.y})")
      println(s"Player Position: (${state.player.x}, ${state.player.y})")

      // Check if player is in explosion
      if (isPlayerInExplosion(bomb.x, bomb.y, state)) {
        println("Player caught in the explosion! 💥 Game Over!")
        return state.copy(gameOver = true)
      }

      // Create a new bomb with the exploded flag set to true
      val explodedBomb = bomb.copy(exploded = true, explosionTurn = Some(state.turns))

      // Remove the bomb from the list of active bombs
      val updatedBombs = state.bombs.filterNot(b => b == bomb)

      // Remove monsters that are affected by the explosion
      val monstersAfterExplosion = state.monsters.filterNot(m =>
        isMonsterInExplosion(m.x, m.y, bomb.x, bomb.y, state)
      )

      // Check if all monsters are dead
      if (monstersAfterExplosion.isEmpty) {
        println("You killed all the monsters! You win!")
        return state.copy(gameOver = true)
      }

      // Update the game state after the explosion
      state.copy(
        monsters = monstersAfterExplosion,
        bombs = updatedBombs :+ explodedBomb,
        explosionClearTurn = Some(state.turns + 1),
        bombPlaced = false
      )
    } else {
      state
    }
  }


  // Check if the monster is within the explosion radius and doesn't hit a wall
  def isMonsterInExplosion(monsterX: Int, monsterY: Int, bombX: Int, bombY: Int, state: GameState): Boolean = {
    // Function to check if the explosion is valid in a given direction
    @tailrec
    def isValidExplosionDirection(dx: Int, dy: Int, x: Int = bombX, y: Int = bombY, distance: Int = 0): Boolean = {
      // Move along the explosion direction recursively
      if (x < 0 || x >= state.grid.length || y < 0 || y >= state.grid(x).length) return false
      if (state.grid(x)(y) == "#") return false // Stop if we hit a wall

      // If the monster is within the explosion range, return true
      if (x == monsterX && y == monsterY) return true

      // If we've reached the explosion radius limit, stop
      if (distance >= state.bombRadius) return false

      // Continue recursively in this direction
      isValidExplosionDirection(dx, dy, x + dx, y + dy, distance + 1)
    }

    // Check all four directions: up, down, left, right
    val directions = Seq(
      (-1, 0), // Left
      (1, 0),  // Right
      (0, -1), // Up
      (0, 1)   // Down
    )

    // The monster is in the explosion if any direction finds it within range
    directions.exists { case (dx, dy) => isValidExplosionDirection(dx, dy) }
  }





  // Mark a cell as exploded if within bounds and not a wall
  def markExplosion(x: Int, y: Int, state: GameState): Unit = {
    if (x >= 0 && x < state.grid.length && y >= 0 && y < state.grid(x).length) {
      state.grid(x)(y) match {
        case "#" => // Do nothing if it's an unbreakable wall
        case "*" =>
          state.grid(x)(y) = "." // Break the wall, turn '*' into '.'
        case _ =>
          state.grid(x)(y) = "X" // Mark the explosion
      }
    }
  }


  // Function to handle marking explosion in one direction (up, down, left, or right)
  def markExplosionInDirection(startX: Int, startY: Int, dx: Int, dy: Int, state: GameState, radius: Int): Unit = {
    // Function to mark the explosion recursively
    def markExplosion(x: Int, y: Int, distance: Int): Unit = {
      // Stop if out of bounds or we hit a wall ('#')
      if (x < 0 || x >= state.grid.length || y < 0 || y >= state.grid(x).length) {
        return
      }

      val currentCell = state.grid(x)(y)

      // If it's a breakable wall, mark it as destroyed and stop in this direction
      if (currentCell == "*" || currentCell == "#") {
        if (currentCell == "*") state.grid(x)(y) = "X" // Destroy breakable wall
        return
      }

      // Mark the current cell as exploded (not a wall or breakable wall)
      state.grid(x)(y) = "X" // Represent explosion with 'X'

      // Stop if the distance exceeds the radius limit
      if (distance >= radius) {
        return
      }

      // Continue recursively in this direction
      markExplosion(x + dx, y + dy, distance + 1)
    }

    // Start the recursive marking of the explosion from the starting point
    markExplosion(startX, startY, 0)
  }



  // Check if the player is caught in the explosion
  def isPlayerInExplosion(bombX: Int, bombY: Int, state: GameState): Boolean = {
    val playerX = state.player.x
    val playerY = state.player.y

    // The player's bomb radius is used instead of the default radius
    val explosionRadius = state.bombRadius

    // Function to check if the explosion is blocked by a wall
    def isExplosionBlocked(x: Int, y: Int): Boolean = {
      if (x < 0 || x >= state.grid.length || y < 0 || y >= state.grid(x).length) return true
      val cell = state.grid(x)(y)
      cell == "#" || cell == "*"  // Blocked by walls or breakable walls
    }

    // Check if the player is within explosion radius, but exclude diagonal directions
    val distance = Math.abs(bombX - playerX) + Math.abs(bombY - playerY)

    // Explosion affects only up to 'explosionRadius' blocks in horizontal or vertical directions
    if (distance > explosionRadius || (bombX != playerX && bombY != playerY)) {
      return false  // Player is outside of explosion range or diagonally adjacent
    }

    // Function to check if the explosion path is clear in either horizontal or vertical direction
    def isPathClear(startX: Int, startY: Int, endX: Int, endY: Int): Boolean = {
      var x = startX
      var y = startY
      while (x != endX || y != endY) {
        if (isExplosionBlocked(x, y)) return false
        if (x < endX) x += 1
        else if (x > endX) x -= 1
        if (y < endY) y += 1
        else if (y > endY) y -= 1
      }
      true
    }

    // Check if the explosion path is blocked for horizontal or vertical directions
    if (bombX == playerX) {
      // Vertical direction check
      if (!isPathClear(bombX, bombY, bombX, playerY)) return false
    } else if (bombY == playerY) {
      // Horizontal direction check
      if (!isPathClear(bombX, bombY, playerX, bombY)) return false
    }

    // If the player is within range and the path is clear, return true
    true
  }









  // Move monsters randomly after the first move
  def moveMonsters(state: GameState): GameState = {
    // If the monsters are frozen, decrement the frozen turns and prevent their movement
    val updatedFrozenTurns = if (state.frozenTurns > 0) state.frozenTurns - 1 else state.frozenTurns

    // Ensure monsters start moving after the freeze period ends
    if (updatedFrozenTurns > 0) {
      return state.copy(frozenTurns = updatedFrozenTurns) // Monsters remain frozen, no movement happens
    }

    // Proceed to move monsters if they're not frozen
    val random = new Random()

    // Get the bomb locations (for checking)
    val bombLocations = state.bombs.filterNot(_.exploded).map(bomb => (bomb.x, bomb.y)).toSet

    // Update each monster's position immutably
    val updatedMonsters = state.monsters.map { monster =>
      // Calculate the direction to move towards the player
      val (dx, dy) = (state.player.x - monster.x, state.player.y - monster.y)

      // Determine the monster's move direction, but only move vertically or horizontally
      val (preferredMoveX, preferredMoveY) = {
        // Prioritize horizontal movement if necessary
        if (Math.abs(dx) > Math.abs(dy)) {
          (dx.sign, 0) // Move horizontally (left or right)
        } else {
          (0, dy.sign) // Move vertically (up or down)
        }
      }

      // List of possible move directions (X, Y): right, left, down, up
      val possibleMoves = List(
        (1, 0),  // Move right
        (-1, 0), // Move left
        (0, 1),  // Move down
        (0, -1)  // Move up
      )

      // Function to check if a move is valid (no wall or bomb)
      def isValidMove(x: Int, y: Int): Boolean = {
        x >= 0 && x < state.grid.length &&
          y >= 0 && y < state.grid(x).length &&
          state.grid(x)(y) != "#" && state.grid(x)(y) != "*" && !bombLocations.contains((x, y))
      }

      // Try the preferred move first
      val (newMoveX, newMoveY) = if (isValidMove(monster.x + preferredMoveX, monster.y + preferredMoveY)) {
        (preferredMoveX, preferredMoveY)
      } else {
        // If the preferred move is blocked, try all possible moves
        val validMoves = possibleMoves.filter { case (moveX, moveY) =>
          isValidMove(monster.x + moveX, monster.y + moveY)
        }

        // If there are valid moves, randomly pick one; otherwise, stay in place
        if (validMoves.nonEmpty) validMoves(random.nextInt(validMoves.length)) else (0, 0)
      }

      // Calculate the new position based on the move
      val newX = monster.x + newMoveX
      val newY = monster.y + newMoveY

      // Update the monster's position
      monster.copy(x = newX, y = newY)
    }

    // Update the game state with the new list of monsters and the updated frozen turns
    state.copy(monsters = updatedMonsters, frozenTurns = updatedFrozenTurns)
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
        // If the game is over (either player lost or won), print the result
        if (state.monsters.isEmpty) {
          IO(println("You killed all the monsters! You win!"))
        } else {
          IO(println("Game Over!"))
        }
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
                if (updatedState == finalState) {
                  // If the move was invalid, prompt again
                  loop(finalState)
                } else {
                  // Set firstMove to true after the player has made their first move
                  val stateWithFirstMove = updatedState.copy(firstMove = true)
                  // After player moves, increment the turn and move monsters
                  val updatedTurnState = stateWithFirstMove.copy(turns = stateWithFirstMove.turns + 1)
                  val newStateAfterMonsters = moveMonsters(updatedTurnState) // Move monsters after player move
                  // Check bombs after turn increment
                  val stateWithBombsChecked = checkBombs(newStateAfterMonsters)
                  // Reset bombPlacedThisTurn flag for the next round
                  loop(stateWithBombsChecked.copy(bombPlacedThisTurn = false)) // Continue to next turn after valid input
                }

              case "E" | "e" =>
                // Handle bomb placement
                placeBomb(finalState).flatMap { updatedState =>
                  // Only increment the turn if the bomb was placed successfully
                  if (updatedState != finalState) {
                    // Bomb was placed successfully, so increment turn
                    val updatedTurnState = updatedState.copy(turns = updatedState.turns + 1)
                    val stateWithBombsChecked = checkBombs(updatedTurnState)
                    // Reset bombPlacedThisTurn flag for the next round
                    loop(stateWithBombsChecked)
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
    val monsters = List(Monster(5, 18),Monster(1, 1))  // Two monsters at initial positions
    GameState(Player(2, 18), monsters, initialGrid, List(), turns = 0, bombPlacedThisTurn = false)
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