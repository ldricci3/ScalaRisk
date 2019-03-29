package models
import akka.event.Logging.Info

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
      val (base, neighbors) = extractAdjacencyInfo(line)
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
  private def extractAdjacencyInfo(info: String): (String, List[String]) = {
    val listTokens: List[String] = info.split(", ").toList
    (listTokens.head, listTokens.tail)
  }

  /**
    * Gets AdjacencySet for given territory
    * @param baseTerritory the territory which we want to get the neighbors of.
    * @return the set of territories adjacent to baseTerritory
    */
  def getNeighborsByName(baseTerritory: String): mutable.Set[String] = adjacencySet(baseTerritory)

  /** Loads continent information from source file. */
  def setupContinentsAndTerritories(): Unit = {
    val lines: Array[String] = continentSource.getLines.toArray
    lines.foreach(ln => createContinentTerritories(ln))
  }
  private def createContinentTerritories(info: String): Unit = {
    val (continentName, bonusArmies, territoryNames) = extractContinentInfo(info)
    createContinent(continentName, bonusArmies, territoryNames)
    createTerritories(continentName, territoryNames)
  }
  private def extractContinentInfo(info: String): (String, Int, List[String]) = {
    val tokens: List[String] = info.split(", ").toList
    (tokens.head, tokens.tail.head.toInt, tokens.tail.tail)
  }
  private def createContinent(name: String, armies: Int, territories: List[String]): Unit = {
    continentMap += (name -> new Continent(name, armies, territories))
  }
  private def createTerritories(continent: String, names: List[String]): Unit = {
    names.foreach(name => createTerritory(continent, name))
  }
  private def createTerritory(continent: String, info: String): Unit = {
    val tokens: Array[String] = info.split(" ")
    territoryMap += (tokens(0) -> new Territory(tokens(0), continent, (tokens(1).toDouble.toInt, tokens(2).toDouble.toInt)))
  }

  /**
    * Getter for a single Territory object
    * @param name name of Territory
    * @return the territory object
    */
  def getTerritoryByName(name: String): Territory = territoryMap(name)

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
