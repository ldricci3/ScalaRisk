package models

class Game(val names:  List[String], val colors: List[String]) {
  val requirements: Boolean =
    names.size >= 3 &&
    names.size <= 6 &&
    names.size == colors.size
  require(requirements, "Must have from 3 to 6 players")

  private var baseArmy: Set[Army] = Set()
  for (i <- 1 to (50 - 5 * names.size)) {
    baseArmy += new Infantry
  }

  val numPlayers: Int = names.length
  val namesAndColors: List[(String, String)] = names zip colors
  val players: List[Player] = for (((name, color), id) <- namesAndColors.zipWithIndex) yield {
    new Player(id, color, name, baseArmy)
  }
  var playerOrder: List[Player] = scala.util.Random.shuffle(players)

  val currentTurn: Int = 0
  val gameMap: GameMap = new GameMap

  def gameInProgress: Boolean = {
    var inProgress: Boolean = false
    var continentControllers: Set[Player] = Set.empty[Player]
    for ((continent, players) <- gameMap.controlledBy) {
      if (players.size > 1) {
        inProgress = true
      } else if (players.size == 1) {
        players.head.allocateMoreArmies(Continents.armyAllotment(continent))
        continentControllers += players.head
        inProgress = continentControllers.size != 1
      } else {
        println(s"Error: $continent is empty.")
      }
    }
    inProgress
  }
  while (gameInProgress) {
    val currentPlayer = playerOrder(currentTurn % numPlayers);
    currentPlayer.placeArmies()
    currentPlayer.attack()
    currentPlayer.fortify()
  }
}



