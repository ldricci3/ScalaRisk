package models
import scala.collection.mutable

class Player(val id: Int,
             val color: String,
             val name: String,
             val armies: Set[Army]) {
  /**
  val requirements: Boolean = (color == "Red"
    || color == "Black"
    || color == "Yellow"
    || color == "Green"
    || color == "Blue")
  require(requirements, "Red, Black, Yellow, Green, Blue")
    */

  def this(id: Int, name: String, armies: Set[Army]) = this(id, "Colorless", name, armies)

  var currentAction: Int = 1
  var previousAction: Int = 1
  var territoryNames: List[String] = List()
  var continentNames: List[String] = List()
  val colorRBG: (Int, Int, Int) = createRGB
  var armiesOnReserve: Int = armies.size
  var visited: mutable.Map[String, Boolean] = mutable.Map.empty[String, Boolean]
  var connectedTerritories: mutable.Set[String] = mutable.Set.empty[String]

  /** Assign territories to this player at the start of game. */
  def assignInitialTerritories(initialTerritories: List[String]): Unit = {
    territoryNames = initialTerritories
  }

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

  private def isOwnedTerritory(name: String): Boolean = territoryNames.contains(name)

  /** Let this player attack unoccupied neighboring territories */
  def attack(): Unit = {
    currentAction = 2
      //if attack is successful
      //check if player has complete ownership of the continent.
      //add continent name to continentNames
    previousAction = currentAction
  }

  /** Fills visited Map values with false */
  def resetVisited(): Unit = {
    visited = mutable.Map[String, Boolean]().withDefaultValue(false)
  }

  /**
    * Depth first search on adjacent territories
    * saves the connected component containing
    *  the given territory
    * @param currentTerritory the territory to expand from
    */
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
  def fortify(): Unit = {
    currentAction = 3
    previousAction = currentAction
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
