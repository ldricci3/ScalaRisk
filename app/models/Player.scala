package models

class Player(val id: Int, val color: String, val name: String, val armies: Set[Army]) extends hasDice {
  val colorRBG: Any = createRGB
  def createRGB: Any = color match {
    case "Red" => (255, 0, 0)
    case "Black" => (0, 0, 0)
    case "Yellow" => (255, 255, 0)
    case "Green" => (0, 128, 0)
    case "Blue" => (0, 0, 255)
    case _ => None
  }

  var armiesOnReserve = 0
  def allocateMoreArmies(numNewArmies: Int): Unit = {
    armiesOnReserve += numNewArmies
  }

  def placeArmies(): Unit = {
    val value = rollOnce()
    println(s"testing roll: $value")
  }

  def attack(): Unit = {

  }

  def fortify(): Unit = {

  }
}