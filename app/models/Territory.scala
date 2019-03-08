package models

class Territory(val name: String,
                val continent: String,
                var numArmies: Int,
                val position: (Int, Int)) {
  var occupant: Player = new Player(2340, "Red", "Scala", Set.empty[Army])

  /**
    * Constructor used during GameMap setup
    * numArmies defaults to 1
    * this will be initialized later
    *   when deciding initial territory distribution.
    *
    * @param name name of territory
    * @param continent continent that contains this territory
    * @return
    */
  def this(name: String, continent: String, position: (Int, Int)) =
    this(name, continent, 1, position)

  /**
    * Setter for occupant
    * @param newOccupant new occupant
    */
  def setOccupant(newOccupant: Player): Unit = {
    this.occupant = newOccupant
  }

  /**
    * Getter for occupant
    * @return occupant
    */
  def getOccupant: Player = occupant

  /**
    * Checks if aPlayer is the occupant
    * @param aPlayer test on this player
    * @return whether or not aPlayer is the occupant
    */
  def isOccupiedBy(aPlayer: Player): Boolean = {
    aPlayer.equals(this.occupant)
  }

  /** Place more armies on this territory. */
  def addArmies(n: Int): Unit = {
    numArmies = numArmies + n
  }
}
