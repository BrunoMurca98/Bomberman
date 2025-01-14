package Bomberman

case class Bomb (
             x: Int,
             y: Int,
             turnsUntilExplosion: Int,
             exploded: Boolean = false,
             explosionTurn: Option[Int] = None,
             turnPlaced: Int
           ){
  def explodeBomb(state: GameState): GameState = {
    if (!this.exploded) {
      // Mark explosion
      state.markExplosion(this.x, this.y, state)
      state.markExplosionInDirection(this.x, this.y, -1, 0, state, state.bombRadius) // Left
      state.markExplosionInDirection(this.x, this.y, 1, 0, state, state.bombRadius)  // Right
      state.markExplosionInDirection(this.x, this.y, 0, -1, state, state.bombRadius) // Up
      state.markExplosionInDirection(this.x, this.y, 0, 1, state, state.bombRadius)  // Down

      // Debug print to verify player and explosion positions
      println(s"Explosion at: (${this.x}, ${this.y})")
      println(s"Player Position: (${state.player.x}, ${state.player.y})")

      // Check if player is in explosion
      if (state.isPlayerInExplosion(this.x, this.y, state)) {
        println("Player caught in the explosion!")
        return state.copy(gameOver = true)
      }

      // Create a new bomb with the exploded flag set to true
      val explodedBomb = this.copy(exploded = true, explosionTurn = Some(state.turns))

      // Remove the bomb from the list of active bombs
      val updatedBombs = state.bombs.filterNot(b => b == this)

      // Remove monsters that are affected by the explosion
      val monstersAfterExplosion = state.monsters.filterNot(m =>
        state.isMonsterInExplosion(m.x, m.y, this.x, this.y, state)
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

}
