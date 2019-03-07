package models

class Continent(val name: String,
                val bonusArmyAllotment: Int,
                val occupancy: Int,
                val territoryNames: List[String]) {
  var occupantNames: Set[String] = Set.empty[String]
  /**
    * Constructor Chaining.
    * occupancy defaults to -1.
    * we will override later when configuring the initial placement of players.
    * @param name name of continent
    * @param bonusArmyAllotment number of armies granted if bonus is applicable
    * @param territoryNames list of the names of territories.
    * @return
    */
  def this(name: String, bonusArmyAllotment: Int, territoryNames: List[String]) =
    this(name, bonusArmyAllotment, -1, territoryNames)

  /**
    * Determines if a continent has only 1 occupant
    * @return whether or not continent is controlled by a single player
    */
  def isControlled: Boolean = {
    occupancy == 1
  }

  /**
    * Getter for name
    * @return name
    */
  def getName: String = name

  /**
    * Getter for army bonus
    * @return bonusArmyAllotment
    */
  def getArmyAllotment: Int = bonusArmyAllotment

  /**
    * Getter for occupancy
    * occupancy = # distinct players occupying
    *     territory within this continent
    * @return occupancy
    */
  def getOccupancy(): Int = occupantNames.size

  /**
    * Getter for territories
    * @return territoryNames
    */
  def getTerritoryNames: List[String] = territoryNames
}
