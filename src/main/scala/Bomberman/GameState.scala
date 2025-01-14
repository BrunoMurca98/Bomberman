package Bomberman

import cats.effect.IO
import scala.annotation.tailrec

case class GameState(
                     player: Player,
                     monsters: List[Monster],
                     eliteMonsters: List[EliteMonster],
                     grid: Array[Array[String]],
                     bombs: List[Bomb],
                     turns: Int = 0,
                     gameOver: Boolean = false,
                     firstMove: Boolean = false,
                     explosionClearTurn: Option[Int] = None,
                     bombPlaced: Boolean = false,
                     bombPlacedThisTurn: Boolean = false,
                     bombRadius: Int = 2,
                     frozenTurns: Int = 0 ,  // Tracks the number of turns monsters are frozen
                     maxBombs: Int = 1
                    ){

  // Place a bomb at the player's position (side-effectful)
  def placeBomb(state: GameState): IO[GameState] = IO {
    if (state.bombs.count(b => !b.exploded) >= state.maxBombs) {
      println(s"You can only place ${state.maxBombs} bomb(s) at a time! Wait for the previous one(s) to explode.")
      state // Don't place a new bomb, just return the current state
    } else {
      // Set the bombPlaced flag for the current turn
      val newBomb = Bomb(state.player.x, state.player.y, turnsUntilExplosion = 5, turnPlaced = state.turns)  // Set explosion delay to 4 turns
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
          bomb.explodeBomb(updatedState) // Explode the bomb and update the state
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
    @tailrec
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


  def isPlayerNearMonster(playerX: Int, playerY: Int, monsters: List[Monster], eliteMonsters: List[EliteMonster]): Boolean = {
    // Check if the player is near any regular monster
    val isNearRegularMonster = monsters.exists { monster =>
      (playerX == monster.x && (playerY == monster.y - 1 || playerY == monster.y + 1)) || // Vertically adjacent
        (playerY == monster.y && (playerX == monster.x - 1 || playerX == monster.x + 1))   // Horizontally adjacent
    }

    // Check if the player is near any elite monster
    val isNearEliteMonster = eliteMonsters.exists { eliteMonster =>
      (playerX == eliteMonster.x && (playerY == eliteMonster.y - 1 || playerY == eliteMonster.y + 1)) || // Vertically adjacent
        (playerY == eliteMonster.y && (playerX == eliteMonster.x - 1 || playerX == eliteMonster.x + 1)) ||   // Horizontally adjacent
          (playerX == eliteMonster.x - 1 && playerY == eliteMonster.y - 1) || // Top-left diagonal
            (playerX == eliteMonster.x + 1 && playerY == eliteMonster.y - 1) || // Top-right diagonal
              (playerX == eliteMonster.x - 1 && playerY == eliteMonster.y + 1) || // Bottom-left diagonal
                (playerX == eliteMonster.x + 1 && playerY == eliteMonster.y + 1)   // Bottom-right diagonal
    }

    // Return true if the player is near any monster (regular or elite)
    isNearRegularMonster || isNearEliteMonster
  }


}
