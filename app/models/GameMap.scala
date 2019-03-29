package models
import scala.io.Source
import scala.collection.mutable

object GameMap {
  val mapType: String = GameMapType.mapType
  val validMapTypes: Boolean =
    mapType == "basic" || mapType == "test"
  require(validMapTypes, "map type must be either \"basic\" or \"test\"")

  var (continentSource, adjacencySource): (Source, Source) = getResources
  var adjacencySet: mutable.Map[String, mutable.Set[String]] = mutable.Map.empty[String, mutable.Set[String]]

  var territoryMap: mutable.Map[String, Territory] = mutable.Map.empty[String, Territory]
  var continentMap: mutable.Map[String, Continent] = mutable.Map.empty[String, Continent]

  /**
    * Getter for Territory objects
    * @return all Territory objects
    */
  def getTerritories: List[Territory] = territoryMap.values.toList

  /**
    * Getter for Continent objects
    * @return all Continent objects
    */
  def getContinents: List[Continent] = continentMap.values.toList

  /**
    * Get adjacent territories of given territory
    * @return Set of Territory
    */
  def getAdjacentTerritories(homeTerritory: String): mutable.Set[String] = {
    adjacencySet(homeTerritory)
  }

  /**
    * Gets the proper map information for the given mapType
    * @return sources for country and adjacency information
    */
  def getResources: (Source, Source) = mapType match {
    case "basic" => (
      scala.io.Source.fromInputStream(classOf[Nothing].getResourceAsStream("/map-info/continents.txt")),
      scala.io.Source.fromInputStream(classOf[Nothing].getResourceAsStream("/map-info/connections.txt"))
    )
    case "test" => (
      scala.io.Source.fromInputStream(classOf[Nothing].getResourceAsStream("/map-info/test-continents.txt")),
      scala.io.Source.fromInputStream(classOf[Nothing].getResourceAsStream("/map-info/test-connections.txt"))
    )
  }

  /**
    * Loads adjacency information from source file
    *   and sets the adjacentTerritories
    */
  def setupAdjacentTerritories(): Unit = {
    val lines: Array[String] = adjacencySource.getLines().toArray
    for (line <- lines) {
      val tokens: Array[String] = line.split(", ")
      val listTokens: List[String] = tokens.toList
      val base: String = listTokens.head
      val neighbors: List[String] = listTokens.tail

      for (neighbor <- neighbors) {
        if (adjacencySet.contains(base)) {
          adjacencySet(base) += neighbor
        } else {
          adjacencySet += (base -> mutable.Set(neighbor))
        }
        val hackError: Unit = {}
      }
    }
  }

  /**
    * Gets AdjacencySet for given territory
    * @param baseTerritory the territory which we want to get the neighbors of.
    * @return the set of territories adjacent to baseTerritory
    */
  def getNeighborsByName(baseTerritory: String): mutable.Set[String] = {
    adjacencySet(baseTerritory)
  }

  /** Loads continent information from source file. */
  def setupContinentsAndTerritories(): Unit = {
    val lines: Array[String] = continentSource.getLines.toArray
    for (line <- lines) {
      val tokens: Array[String] = line.split(", ")
      val listTokens: List[String] = tokens.toList

      val continentName: String = listTokens.head
      val withoutName: List[String] = listTokens.tail
      val bonusArmies: Int = withoutName.head.toInt
      val territoryNames: List[String] = withoutName.tail

      if (continentMap.contains(continentName)) {
        println(s"File Content Error: $continentName found in different lines.")
      } else {
        continentMap += (continentName ->
          new Continent(continentName, bonusArmies, territoryNames))
        for (territoryName: String <- territoryNames) {
          if (territoryMap.contains(territoryName)) {
            println(s"File Content Error: $territoryName appears more than once in a single line.")
          } else {
            val info: Array[String] = territoryName.split(" ")
            territoryMap += (info(0) -> new Territory(info(0), continentName, (info(1).toDouble.toInt, info(2).toDouble.toInt)))
          }
        }
      }
    }
  }

  /**
    * Getter for a single Territory object
    * @param name name of Territory
    * @return the territory object
    */
  def getTerritoryByName(name: String): Territory = {
    territoryMap(name)
  }

  /**
    * Getter for a single Territory object
    * @param names names of Territories
    * @return the territory object
    */
  def getTerritoriesByNames(names: List[String]): List[Territory] = {
    var territories: List[Territory] = List.empty[Territory]
    for (name:String <- names) {
      territories = getTerritoryByName(name)::territories
    }
    territories
  }
}
