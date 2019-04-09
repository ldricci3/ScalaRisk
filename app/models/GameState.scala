package models

sealed trait GameState {
  def state: String
}
case object Place extends GameState { val state = "PLACE" }
case object Attack extends GameState { val state = "ATTACK" }
case object Roll extends GameState { val state = "ROLL" }
case object Defend extends GameState { val state = "DEFEND" }
case object Fortify extends GameState { val state = "FORTIFY" }