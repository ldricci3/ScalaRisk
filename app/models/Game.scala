package models

class Game(val names:  List[String], val colors: List[String]) {
  val requirements: Boolean =
    names.size >= 3 &&
    names.size <= 6 &&
    names.size == colors.size
  require(requirements, "Must have from 3 to 6 players")

  /** R3: Assigning initial allottment of armies */
  val numPlayers: Int = names.length
  private var baseArmy: Set[Army] = Set()
  for (i <- 1 to (50 - 5 * names.size)) {
    baseArmy += new Infantry
  }
  /** R1: 3-6 uniquely identifiable players */
  val namesAndColors: List[(String, String)] = names zip colors
  val players: List[Player] = for (((name, color), id) <- namesAndColors.zipWithIndex) yield {
    new Player(id, color, name, baseArmy)
  }
  /** R2: Random turn order */
  val playerOrder: List[Player] = scala.util.Random.shuffle(players)
  val currentTurn: Int = 0

  /** Loads map, continent, territory data. */
  def setupGameMap(): Unit = {
    GameMapType.mapType = "basic"
    GameMap.getResources
    GameMap.setupAdjacentTerritories()
  }

  val M1only: Boolean = true
  if (!M1only) {
    /**
      * Checks if all continents are controlled by a single player
      * Also grants continent army bonus
      * @return whether game is still in progress
      */
    def gameInProgress: Boolean = {
      var inProgress: Boolean = false
      var continentControllers: Set[Player] = Set.empty[Player]
      for (c: Continent <- GameMap.getContinents) {
        if (c.getOccupancy == 1) {
          val aTerritory = c.getTerritoryNames.head
          val bonus: Int = c.getArmyAllotment
          GameMap.territoryMap(aTerritory).getOccupant.allocateMoreArmies(bonus)

          continentControllers += GameMap.territoryMap(aTerritory).getOccupant
          if (continentControllers.size > 1) {
            inProgress = true
          }
        } else {
          inProgress = true
        }
      }
      inProgress
    }
    while (gameInProgress) {
      val currentPlayer = playerOrder(currentTurn % numPlayers)
      currentPlayer.placeArmies()
      currentPlayer.attack()
      currentPlayer.fortify()
    }
  }
}



