import scala.io.StdIn.readLine
import java.util.{Timer, TimerTask}

// Directions for movement
val directions = Map(
  'w' -> (-1, 0), // Up
  's' -> (1, 0),  // Down
  'a' -> (0, -1), // Left
  'd' -> (0, 1)   // Right
)

// Player representation
case class Player(var x: Int, var y: Int)

// Bomb representation with timestamp for placement
case class Bomb(var x: Int, var y: Int, var placedAt: Long, var exploded: Boolean = false)

object Bomberman {
  // The game map, represented as a 2D array of chars
  val grid: Array[Array[Char]] = Array(
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '#', '#', '#', '.', '#', '#', '.', '#', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '.', '.', '.', '.', '#', '.', '.', '.', '#'),
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#')
  )

  // List to track bombs
  var bombs: List[Bomb] = List()

  // Display the grid
  def displayGrid(player: Player): Unit = {
    println("Bomberman Game:")
    for (i <- 0 until grid.length) {
      for (j <- 0 until grid(i).length) {
        // If it's the player's position, mark it as '1'
        if (i == player.x && j == player.y) {
          print(" 1 ")
        } else {
          print(s" ${grid(i)(j)} ")
        }
      }
      println()
    }
  }

  // Handle player movement
  def movePlayer(player: Player, direction: Char): Unit = {
    directions.get(direction) match {
      case Some((dx, dy)) =>
        val newX = player.x + dx
        val newY = player.y + dy
        if (newX >= 0 && newX < grid.length && newY >= 0 && newY < grid(newX).length && grid(newX)(newY) != '#') {
          // Clear the player's old position
          grid(player.x)(player.y) = '.'

          // Update the player's position
          player.x = newX
          player.y = newY

          // Mark the new position with the player (1)
          grid(player.x)(player.y) = '1'
        }
      case None => println("Invalid direction!")
    }
  }

  // Place a bomb at the player's position
  def placeBomb(player: Player): Unit = {
    // Allow bomb placement even if the player's position is marked as '1'
    val currentTime = System.currentTimeMillis()
    val newBomb = Bomb(player.x, player.y, currentTime)
    bombs = newBomb :: bombs // Add bomb with the current time

    // Place the bomb at the player's current position
    grid(player.x)(player.y) = 'B' // Place the bomb on the grid at the player's position

    println(s"Bomb placed at (${player.x}, ${player.y})")

    // Schedule the bomb to explode after 3 seconds
    val timer = new Timer()
    val timerTask = new TimerTask {
      def run(): Unit = {
        // Explode the bomb after 3 seconds
        explodeBomb(newBomb, player)
      }
    }
    timer.schedule(timerTask, 3000) // Schedule to run after 3 seconds
  }

  // Explosion logic (with radius of 1 on both X and Y axes)
  def explodeBomb(bomb: Bomb, player: Player): Unit = {
    if (!bomb.exploded) {
      // Mark the bomb location with 'X' to represent explosion
      markExplosion(bomb.x, bomb.y) // Mark the bomb's own location

      // Mark adjacent cells in the explosion radius (up, down, left, right)
      markExplosion(bomb.x - 1, bomb.y) // Up
      markExplosion(bomb.x + 1, bomb.y) // Down
      markExplosion(bomb.x, bomb.y - 1) // Left
      markExplosion(bomb.x, bomb.y + 1) // Right

      bomb.exploded = true // Mark as exploded
      println(s"Bomb exploded at (${bomb.x}, ${bomb.y})")

      // Check if the player is caught in the explosion
      if (checkGameOver(player)) { // Check if the player is caught
        println("You were caught in an explosion! Game over.")
        System.exit(0) // Exit the game immediately
      }


    }
  }

  // Mark a cell as exploded if within bounds
  def markExplosion(x: Int, y: Int): Unit = {
    if (x >= 0 && x < grid.length && y >= 0 && y < grid(x).length && grid(x)(y) != '#') {
      grid(x)(y) = 'X' // Mark the cell as exploded
    }
  }


  // Check if the bomb exploded at the same position as the player
  def checkGameOver(player: Player): Boolean = {
    // Loop over all bombs and check if the player is at the explosion location
    bombs.exists { bomb =>
      // If the bomb exploded and the player is in any of the explosion radius cells (bomb + adjacent)
      (bomb.exploded &&
        (bomb.x == player.x && bomb.y == player.y || // Player is at bomb location
          bomb.x - 1 == player.x && bomb.y == player.y || // Player is up from bomb
          bomb.x + 1 == player.x && bomb.y == player.y || // Player is down from bomb
          bomb.x == player.x && bomb.y - 1 == player.y || // Player is left of bomb
          bomb.x == player.x && bomb.y + 1 == player.y)) // Player is right of bomb
    }
  }

  // Read a single key press (E or other)
  def readKeyPress(): String = {
    val input = readLine().trim
    input
  }

  // Main game loop
  def mainLoop(): Unit = {
    val player = Player(1, 1) // Starting player position (1, 1)

    // Initialize the grid with walls and set the player's starting position
    grid(player.x)(player.y) = '1' // Mark the player's starting position with '1'

    // Game loop
    while (true) {
      // Display the grid with player as '1'
      displayGrid(player)

      println("Enter move (w/a/s/d to move, E to place bomb):")

      // Read user input (single key press)
      val input = readKeyPress()

      input match {
        case "w" | "a" | "s" | "d" => movePlayer(player, input.head) // Only pass the first character
        case "E" | "e" => placeBomb(player) // E to place bomb (case-insensitive)
        case _ => println("Invalid command!")
      }

      // Compute gameOver condition
      val gameOver = checkGameOver(player)

      if (gameOver) {
        println("You were caught in an explosion! Game over.")
        return // End the game if the player is caught in an explosion
      }
    }
  }
}
object BombermanApp extends App {
  Bomberman.mainLoop() // Start the game loop
}
