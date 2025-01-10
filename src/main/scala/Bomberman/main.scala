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

// Bomb representation with additional turnPlaced field
case class Bomb(x: Int, y: Int, turnsUntilExplosion: Int, exploded: Boolean = false, explosionTurn: Option[Int] = None, turnPlaced: Int)


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
                      bombPlaced: Boolean = false,
                      bombPlacedThisTurn: Boolean = false,
                      bombRadius: Int = 2  // Default bomb radius is 2
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
  val initialGrid: Array[Array[Char]] = Array(
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#' ,'#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.' , '.', '.', '.', '.', '.', '.', '.', '.','.', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.' , '.', '.', '.', '.', '*', '.', '.', '#','.', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.' , '.', '.', '*', '*', '.', '*', '*', '#','.', '#'),
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
        // Show the bomb on the grid only if it hasn't exploded
        val bombToDisplay = state.bombs.find(bomb => bomb.x == i && bomb.y == j && !bomb.exploded)
        bombToDisplay match {
          case Some(bomb) if state.turns - bomb.turnPlaced >= 2 =>
            // Mark the bomb location with 'B' if it's been 2 turns since placement and it hasn't exploded
            print(" B ")
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
          state.grid(state.player.x)(state.player.y) = '.'  // Clear the old position

          // Check if the new position is a valid move (not a wall, breakable wall, or bomb)
          if (state.grid(newX)(newY) != '#' && state.grid(newX)(newY) != '*' && !state.bombs.exists(b => b.x == newX && b.y == newY && !b.exploded)) {
            val newPlayer = state.player.copy(x = newX, y = newY)

            // If the player lands on a power-up, increase bomb radius
            if (state.grid(newX)(newY) == 'F') {
              println("Power-Up Collected! Your bomb radius has increased! 💥")
              state.grid(newX)(newY) = '.'  // Remove the power-up from the grid
              return state.copy(player = newPlayer, bombRadius = state.bombRadius + 1)
            }

            state.grid(newX)(newY) = '1'  // Mark the new position of the player
            state.copy(player = newPlayer)
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


  def explodeBomb(bomb: Bomb, state: GameState): GameState = {
    if (!bomb.exploded) {
      // Mark the bomb location with 'X' to represent explosion
      markExplosion(bomb.x, bomb.y, state)

      // Explosions in 4 directions with the current bomb radius
      markExplosionInDirection(bomb.x, bomb.y, -1, 0, state, state.bombRadius) // Left
      markExplosionInDirection(bomb.x, bomb.y, 1, 0, state, state.bombRadius)  // Right
      markExplosionInDirection(bomb.x, bomb.y, 0, -1, state, state.bombRadius) // Up
      markExplosionInDirection(bomb.x, bomb.y, 0, 1, state, state.bombRadius)  // Down

      // Create a new Bomb with exploded = true and the current explosion turn
      val explodedBomb = bomb.copy(exploded = true, explosionTurn = Some(state.turns))

      // Remove the bomb from the list of bombs (bomb has exploded)
      val updatedBombs = state.bombs.filterNot(b => b == bomb)

      // Remove monsters that are within the explosion radius, considering walls
      val monstersAfterExplosion = state.monsters.filterNot(m =>
        isMonsterInExplosion(m.x, m.y, bomb.x, bomb.y, state)
      )

      // Check if the player is in the explosion radius
      if (isPlayerInExplosion(bomb.x, bomb.y, state)) {
        println("Player caught in the explosion!💥")
        return state.copy(gameOver = true) // Set the game to over and return the updated state
      }

      // If no monsters are left, the player wins!
      if (monstersAfterExplosion.isEmpty) {
        println("You killed all the monsters! You win!")
        return state.copy(gameOver = true) // Set the game to over and declare victory
      }

      // Reset the bombPlaced flag after the bomb explodes
      state.copy(
        monsters = monstersAfterExplosion,  // Update monsters list after explosion
        bombs = updatedBombs :+ explodedBomb,  // Add the exploded bomb to the list of bombs
        explosionClearTurn = Some(state.turns + 1), // Set the turn when to clear the explosion
        bombPlaced = false // Allow placing a new bomb after the explosion
      )
    } else {
      state // If the bomb has already exploded, return the state as is
    }
  }


  // Check if the monster is within the explosion radius and doesn't hit a wall
  def isMonsterInExplosion(monsterX: Int, monsterY: Int, bombX: Int, bombY: Int, state: GameState): Boolean = {
    // Function to check if the explosion is valid in a given direction
    def isValidExplosionDirection(dx: Int, dy: Int): Boolean = {
      var x = bombX
      var y = bombY

      while (x >= 0 && x < state.grid.length && y >= 0 && y < state.grid(x).length) {
        x += dx
        y += dy

        if (x == monsterX && y == monsterY) return true // Found monster in explosion radius

        // Stop if we hit a wall
        if (state.grid(x)(y) == '#') return false
      }
      false
    }

    // Check if the monster is in explosion radius considering walls
    isValidExplosionDirection(-1, 0) ||  // Left
      isValidExplosionDirection(1, 0) ||   // Right
      isValidExplosionDirection(0, -1) ||  // Up
      isValidExplosionDirection(0, 1)      // Down
  }



  // Mark a cell as exploded if within bounds and not a wall
  def markExplosion(x: Int, y: Int, state: GameState): Unit = {
    if (x >= 0 && x < state.grid.length && y >= 0 && y < state.grid(x).length) {
      state.grid(x)(y) match {
        case '#' => // Do nothing if it's an unbreakable wall
        case '*' =>
          state.grid(x)(y) = '.' // Break the wall, turn '*' into '.'
        case _ =>
          state.grid(x)(y) = 'X' // Mark the explosion
      }
    }
  }


  // Function to handle marking explosion in one direction (up, down, left, or right)
  def markExplosionInDirection(startX: Int, startY: Int, dx: Int, dy: Int, state: GameState, radius: Int): Unit = {
    var x = startX
    var y = startY
    var distance = 0
    var wallDestroyed = false  // Flag to track if a breakable wall has been destroyed

    // Move in the direction (dx, dy) until we hit a wall or reach the radius limit
    while (distance < radius) { // Use radius instead of fixed 2
      x += dx
      y += dy
      distance += 1

      // Stop if out of bounds or if we hit a wall ('#')
      if (x < 0 || x >= state.grid.length || y < 0 || y >= state.grid(x).length || state.grid(x)(y) == '#') {
        return // Stop if it's out of bounds or a wall
      }

      if (state.grid(x)(y) == '*') { // Check if it's a breakable wall
        if (!wallDestroyed) { // Only destroy one breakable wall in this direction
          // Mark the breakable wall as destroyed with 'X'
          state.grid(x)(y) = 'X' // Replace * with X to indicate destruction
          wallDestroyed = true  // Mark that a breakable wall has been destroyed
        }
        return // Stop after destroying the first breakable wall in this direction
      }

      // Mark the cell as exploded (not a wall or breakable wall)
      state.grid(x)(y) = 'X' // Represent explosion with 'X'
    }
  }


  // Check if the player is caught in the explosion
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


  // Move monsters randomly after the first move
  def moveMonsters(state: GameState): GameState = {
    // Ensure monsters start moving after the player makes their first move
    if (!state.firstMove) return state // Don't move monsters until the player moves at least once

    val random = new Random()

    // Get the bomb locations (for checking)
    val bombLocations = state.bombs.filterNot(_.exploded).map(bomb => (bomb.x, bomb.y)).toSet

    // Update each monster's position immutably
    val updatedMonsters = state.monsters.map { monster =>
      val possibleMoves = directions.values.filter { case (dx, dy) =>
        val newX = monster.x + dx
        val newY = monster.y + dy
        newX >= 0 && newX < state.grid.length && newY >= 0 && newY < state.grid(newX).length &&
          state.grid(newX)(newY) == '.' && !bombLocations.contains((newX, newY)) // Ensure monster doesn't move to bomb location
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
    val monsters = List(Monster(5, 18))  // Two monsters at initial positions
    GameState(Player(1, 18), monsters, initialGrid, List(), turns = 0, bombPlacedThisTurn = false)
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