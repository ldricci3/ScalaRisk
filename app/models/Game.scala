package models

class Game(val names:  List[String], val colors: List[String]) {
  val requirements: Boolean =
    names.size >= 3 &&
    names.size <= 6 //&&
    //names.size == colors.size
  require(requirements, "Must have from 3 to 6 players")

  /** R3: Assigning initial allottment of armies */
  val numPlayers: Int = names.length
  private var baseArmy: Set[Army] = Set()
  for (i <- 1 to (50 - 5 * names.size)) {
    baseArmy += new Infantry
  }
  /** R1: 3-6 uniquely identifiable players */
  /** R2: Random turn order */
  val namesAndColors: List[(String, String)] = names.zipAll(colors, "", "")
  val players: List[Player] = scala.util.Random.shuffle(for (((name, color), id) <- namesAndColors.zipWithIndex) yield {
    new Player(id, color, name, baseArmy)
  })

  var currentTurn: Int = 0
  this.setupGameMap()
  this.randomTerritoryAssignment()

  /** Loads map, continent, territory data. */
  def setupGameMap(): Unit = {
    GameMapType.mapType = "basic"
    GameMap.getResources
    GameMap.setupAdjacentTerritories()
    GameMap.setupContinentsAndTerritories()
  }

  def randomTerritoryAssignment(): Unit = {
    var unoccupiedTerritories: scala.collection.Set[String] = GameMap.territoryMap.keySet
    while (unoccupiedTerritories.nonEmpty) {
      val randomPlayer: Player = players(Dice.random.nextInt(players.length))
      val nextTerritoryName: String = unoccupiedTerritories.head
      val nextTerritory: Territory = GameMap.territoryMap(nextTerritoryName)
      val nextContinentName: String  = nextTerritory.continent
      val nextContinent: Continent = GameMap.continentMap(nextContinentName)
      //update player
      randomPlayer.territoryNames = nextTerritoryName :: randomPlayer.territoryNames
      //update territory
      nextTerritory.occupant = randomPlayer
      //update continent
      nextContinent.occupantNames += randomPlayer.name
      //update unoccupiedTerritories
      unoccupiedTerritories = unoccupiedTerritories - nextTerritoryName
    }
  }

  /**
    * Checks if all continents are controlled by a single player
    * Also grants continent army bonus
    * @return whether game is still in progress
    */
  def gameInProgress: Boolean = {
    var inProgress: Boolean = false
    var continentControllers: Set[Player] = Set.empty[Player]
    for (c: Continent <- GameMap.getContinents) {
      if (c.getOccupancy() == 1) {
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
    val currentPlayer = players(currentTurn % numPlayers)
    currentPlayer.placeArmies()
    currentPlayer.attack()
    currentPlayer.fortify()
  }

  override def toString: String = {
    (for (i <- players) yield {
      i.toString()
    }).toString()
  }
}
