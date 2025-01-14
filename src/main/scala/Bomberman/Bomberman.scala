package Bomberman

import cats.effect._
import cats.syntax.all._
import scala.util.Random
import scala.concurrent.duration._
import java.util.Timer
import java.util.TimerTask
import scala.annotation.tailrec


object Bomberman {
  // Directions for movement
  val directions = Map(
    'w' -> (-1, 0), // Up
    's' -> (1, 0),  // Down
    'a' -> (0, -1), // Left
    'd' -> (0, 1)   // Right
  )
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
              if (state.isPlayerNearMonster(newState.player.x, newState.player.y, newState.monsters)) {
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
              if (state.isPlayerNearMonster(newState.player.x, newState.player.y, newState.monsters)) {
                println("A monster is adjacent to you!")
                return newState.copy(gameOver = true)
              }
              return newState
            }

            val newState = state.copy(player = newPlayer)

            // After moving, check if a monster is adjacent and end the game if so
            if (state.isPlayerNearMonster(newState.player.x, newState.player.y, newState.monsters)) {
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
          val updatedState = if (state.isPlayerNearMonster(state.player.x, state.player.y, state.monsters)) {
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
                  val stateWithBombsChecked = state.checkBombs(newStateAfterMonsters)
                  // Reset bombPlacedThisTurn flag for the next round
                  loop(stateWithBombsChecked.copy(bombPlacedThisTurn = false)) // Continue to next turn after valid input
                }

              case "E" | "e" =>
                // Handle bomb placement
                state.placeBomb(finalState).flatMap { updatedState =>
                  // Only increment the turn if the bomb was placed successfully
                  if (updatedState != finalState) {
                    // Bomb was placed successfully, so increment turn
                    val updatedTurnState = updatedState.copy(turns = updatedState.turns + 1)
                    val stateWithBombsChecked = state.checkBombs(updatedTurnState)
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
