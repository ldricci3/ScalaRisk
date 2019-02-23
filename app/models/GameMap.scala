package models

class GameMap extends hasDice {
  val controlledBy: Map[String, Set[Player]] = initializeMap()

  def initializeMap(): Map[String, Set[Player]] = {
    //random placement of armies.
    Map("stub" -> Set(new Player(1, "color", "name", Set())))
  }
}
