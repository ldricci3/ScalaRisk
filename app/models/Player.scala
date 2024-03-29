package models
import scala.collection.mutable

class Player(val id: Int,
             val color: String,
             val name: String,
             val armies: Set[Army],
             val ip: String) {
  /**
  val requirements: Boolean = (color == "Red"
    || color == "Black"
    || color == "Yellow"
    || color == "Green"
    || color == "Blue")
  require(requirements, "Red, Black, Yellow, Green, Blue")
    */

  def this(id: Int, name: String, armies: Set[Army]) = this(id, "Colorless", name, armies, "0")

  var currentAction: Int = 1
  var previousAction: Int = 1
  var territoryNames: List[String] = List()
  var continentNames: List[String] = List()
  val colorRBG: (Int, Int, Int) = createRGB
  var armiesOnReserve: Int = armies.size
  var neighbors: mutable.Set[String] = mutable.Set.empty[String]

  /**
    * Convert color string to RGB
    * @return tuple of rbg values
    */
  def createRGB: (Int, Int, Int) = color match {
    case "Red" => (255, 0, 0)
    case "Black" => (0, 0, 0)
    case "Yellow" => (255, 255, 0)
    case "Green" => (0, 128, 0)
    case "Blue" => (0, 0, 255)
    case "Orange" => (255,165,0)
    case "White" => (255, 255, 255)
    case "Colorless" => (127, 127, 127)
    case "" => (127, 127, 127)
  }

  /**
    * Calculate and give player proper allotment of new armies.
    * This does NOT include the Continent bonus. (calculated in Game.gameInProgress method)
    */
  val MIN_ALLOTMENT = 3
  val NUM_TERRITORY_THRESHOLD = 9
  val TERRITORIES_PER_ARMY = 3
  def allocateTurnAllotment(): Unit = {
    val numT: Int = getNumTerritories()
    //regular allotment
    if (numT < NUM_TERRITORY_THRESHOLD) {
      allocateMoreArmies(MIN_ALLOTMENT)
    } else {
      allocateMoreArmies(numT / TERRITORIES_PER_ARMY)
    }
    //bonus continent allotment
    for (cont <- continentNames) {
      allocateMoreArmies(GameMap.continentMap(cont).bonusArmyAllotment)
    }
  }
  /**
    * Give player more armies
    * @param numNewArmies number of new army drafts
    */
  def allocateMoreArmies(numNewArmies: Int): Unit = {
    armiesOnReserve += numNewArmies
  }

  /** Let this player deploy armies on occupied territories */
  def placeArmies(territory: String, armies: Int): Unit = {
    currentAction = 1
    GameMap.territoryMap(territory).addArmies(armies)
    armiesOnReserve -= armies
    previousAction = currentAction
  }

  def ownsTerritory(name: String): Boolean = territoryNames.contains(name)

  def neighborsWith(name: String): Boolean = neighbors.contains(name)

  def updateNeighbors(): Unit = {
    territoryNames.foreach(addNeighborsOf(_))
    neighbors = neighbors.filter(!ownsTerritory(_))
  }

  def updateNeighbors(name: String): Unit = {
    GameMap.adjacencySet(name).foreach(addNeighbor(_))
    neighbors.filter(!ownsTerritory(_))
  }
  def addNeighborsOf(name: String): Unit = GameMap.adjacencySet(name).foreach(addNeighbor(_))
  def addNeighbor(name: String): Unit = neighbors += name


  /** Let this player attack unoccupied neighboring territories */
  def attack(to: String, from: String, numAttackers: Int, numDefenders: Int): Boolean = {//(List[Int], List[Int]) = {
    currentAction = 2
    val aTerr = GameMap.getTerritoryByName(from)
    val dTerr = GameMap.getTerritoryByName(to)

    val attackRolls = Dice.roll(numAttackers)
    val defendRolls = Dice.roll(numDefenders)
    val minLength = Integer.min(attackRolls.length, defendRolls.length)
    for (i <- 0 until minLength) {
      if (attackRolls(i) > defendRolls(i)) {
        dTerr.addArmies(-1)
      } else {
        aTerr.addArmies(-1)
      }
    }
    var attackerWins = false
    if (dTerr.numArmies == 0) {
      // could I replace aTerr.getOccupant with this?
      // we will see soon.
      dTerr.occupant.territoryNames = dTerr.occupant.territoryNames.filter(x => x != to)
      dTerr.setOccupant(this)
      this.territoryNames = to :: this.territoryNames

      moveArmies(to, from, numAttackers)
      attackerWins = true
    }
    previousAction = currentAction
    //(attackRolls.toList, defendRolls.toList)
    BattleInfo.attackRolls = attackRolls.toList
    BattleInfo.defendRolls = defendRolls.toList
    attackerWins
  }

  /** Allows player to move armies from one territory to another */
  def moveArmies(to: String, from: String, num: Int): Unit = {
    val frum = GameMap.getTerritoryByName(from)
    val two = GameMap.getTerritoryByName(to)

    frum.minusArmies(num)
    two.addArmies(num)
  }


  /**
    * DFS
    */
  var visited: mutable.Map[String, Boolean] = mutable.Map.empty[String, Boolean]
  /** Fills visited Map values with false */
  def resetVisited(): Unit = {
    visited = mutable.Map[String, Boolean]().withDefaultValue(false)
    visited = collection.mutable.Map(GameMap.territoryMap.keySet.toList.map(e => e -> true).toMap.toSeq: _*)
  }

  /**
    * Depth first search on adjacent territories
    * saves the connected component containing
    *  the given territory
    * @param currentTerritory the territory to expand from
    */

  var connectedTerritories: mutable.Set[String] = mutable.Set.empty[String]
  def dfs(currentTerritory: String): Unit = {
    visited(currentTerritory) = true
    connectedTerritories += currentTerritory
    //recursively search neighbors
    val neighbors: mutable.Set[String] = GameMap.getNeighborsByName(currentTerritory)
    neighbors
      .filter((name: String) => !visited(name)
        && GameMap.getTerritoryByName(name).isOccupiedBy(this))
      .foreach(neighbor => dfs(neighbor))
  }

  /** Let this player transfer a certain amount of armies between two occupied and distinct territories. */
  def fortify(to: String, from: String, numArmies: Int): Unit = {
    GameMap.territoryMap(to).addArmies(numArmies)
    GameMap.territoryMap(from).minusArmies(numArmies)
  }

  /**
    * Override equals method
    * @param other the player being compared to this
    * @return equality
    */
  final override def equals(other: Any): Boolean = other match {
    case that: Player => id == that.id && color == that.color && name == that.name
    case _ => false
  }

  /**
    * Override hashcode method
    * unique player identity can be determined based on id, color, and name
    * @return hashcode
    */
  final override def hashCode: Int = (id, color, name).##

  override def toString: String = {
    var output: String = this.name + " has " + this.armiesOnReserve + " armies" + "\n"
    output +
      (for (tName: String <- territoryNames) yield { tName + "\n" }).foldLeft("")((a: String, b: String) => a + b) +
      "\n"
  }

  def getNumTerritories(): Int = this.territoryNames.length
}
