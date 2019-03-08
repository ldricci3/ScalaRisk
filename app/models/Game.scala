package models

import scala.collection.mutable
import scala.io.Source

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
  setupGame()
  randomTerritoryAssignment()

  /** Loads map, continent, territory data. */
  def setupGame(): Unit = {
    GameMapType.mapType = "basic"
    GameMap.getResources
    GameMap.setupAdjacentTerritories()
    GameMap.setupContinentsAndTerritories()
  }

  /**R6: Players have their armies assigned to territories*/
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

    //evenly distribute armies across all occupied territories
    var checkTotal: Int = 0
    for (p: Player <- players) {
      var checkMath: Int = 0
      p.armiesOnReserve = p.armiesOnReserve - 1  //subtract off the armies we place to claim each territory
      val div: Int = p.armiesOnReserve / p.getNumTerritories()
      var rem: Int = p.armiesOnReserve % p.getNumTerritories()
      for (t: String <- p.territoryNames) {
        assert(GameMap.territoryMap(t).occupant.equals(p))
        GameMap.territoryMap(t).addArmies(div)
        checkMath = checkMath + div
      }
      for (ii <- 1 to rem) {
        val tName: String = p.territoryNames(Dice.random.nextInt(p.getNumTerritories()))
        GameMap.territoryMap(tName).addArmies(1)
        checkMath = checkMath + 1
      }
      p.armiesOnReserve = 0
      assert(p.armies.size - 1 == checkMath)
      checkTotal = checkTotal + 1 + checkMath
    }
    val realTotal = players.size match {
      case 3 => 105
      case 4 => 120
      case 5 => 125
      case 6 => 120
    }
    assert(checkTotal == realTotal)
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

  def getCurrentPlayer(): Player = players(currentTurn % numPlayers)

  def runGame(): Unit = {
    while (gameInProgress) {
      val currentPlayer = getCurrentPlayer()
      currentPlayer.placeArmies()
      currentPlayer.attack()
      currentPlayer.fortify()
    }
  }

  override def toString: String = {
    (for (i <- players) yield {
      i.toString()
    }).toString()
  }
}