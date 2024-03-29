package models

class Game() {
  var currentTurn: Int = 0
  var players: List[Player] = List.empty
  var isStarted: Boolean = false
  var attacker: Attacker = Attacker(-1, "","", -1)
  var attackRolls: List[Int] = List.empty[Int]
  var defendRolls: List[Int] = List.empty[Int]


  var state: GameState = Place
  def nextState(): Unit = {
    state = state match {
      case Place => Attack
      case Attack => Fortify
      case Defend => Roll
      case Roll => Attack
      case Fortify => Place
      case End => End
    }
    if (state == Place) {
      nextTurn()
    }
    removeLosingPlayers()
  }

  /** Loads map, continent, territory data. */
  def setupGame(names:  List[String], colors: List[String], ipList: List[String]): Unit = {
    val requirements: Boolean =
      names.size >= 3 && names.size <= 6 //&&
    //names.size == colors.size
    require(requirements, "Must have from 3 to 6 players")

    /** R3: Assigning initial allottment of armies */
    val numPlayers: Int = names.length
    var baseArmy: Set[Army] = Set()
    for (i <- 1 to (50 - 5 * names.size)) yield {
      baseArmy += new Infantry
    }
    /** R1: 3-6 uniquely identifiable players */
    /** R2: Random turn order */
    val namesAndColors: List[(String, String)] = names.zipAll(colors.take(names.length), "", "")
    players = scala.util.Random.shuffle(for (((name, color), id) <- namesAndColors.zipWithIndex) yield {
      new Player(id, color, name, baseArmy, ipList(id))
    })
    GameMapType.mapType = "basic"
    GameMap.getResources
    GameMap.setupAdjacentTerritories()
    GameMap.setupContinentsAndTerritories()

    if (names.contains("ENDGAMETEST")) {
      testEndGame()
    } else {
      randomTerritoryAssignment()
    }

    isStarted = true

    getCurrentPlayer().allocateTurnAllotment()
  }

  def testEndGame(): Unit = {
    val winner = players.head
    val loser = players.last

    var unoccupiedTerritories: scala.collection.immutable.List[String] =
      scala.util.Random.shuffle(GameMap.territoryMap.keySet.toList)

    unoccupiedTerritories = assignTerritory(unoccupiedTerritories, loser)
    while (unoccupiedTerritories.nonEmpty) {
      unoccupiedTerritories = assignTerritory(unoccupiedTerritories, winner)
    }

    for (p <- players) {
      p.updateNeighbors()
      p.armiesOnReserve = 0
    }

  }
  def assignTerritory(terries: scala.collection.immutable.List[String], receiver: Player): scala.collection.immutable.List[String] = {
    var unoccupiedTerritories = terries
    val nextTerritoryName: String = unoccupiedTerritories.head
    val nextTerritory: Territory = GameMap.territoryMap(nextTerritoryName)
    val nextContinentName: String  = nextTerritory.continent
    val nextContinent: Continent = GameMap.continentMap(nextContinentName)
    receiver.territoryNames = nextTerritoryName :: receiver.territoryNames
    nextTerritory.occupant = receiver
    nextContinent.occupantNames += receiver.name
    unoccupiedTerritories = unoccupiedTerritories.tail
    unoccupiedTerritories
  }

  /**R6: Players have their armies assigned to territories*/
  def randomTerritoryAssignment(): Unit = {
    var unoccupiedTerritories: scala.collection.immutable.List[String] =
      scala.util.Random.shuffle(GameMap.territoryMap.keySet.toList)
    var i: Int = 0
    while(unoccupiedTerritories.nonEmpty) {
      val currPlayer: Player = players(i % players.length)
      val nextTerritoryName: String = unoccupiedTerritories.head
      val nextTerritory: Territory = GameMap.territoryMap(nextTerritoryName)
      val nextContinentName: String  = nextTerritory.continent
      val nextContinent: Continent = GameMap.continentMap(nextContinentName)
      currPlayer.territoryNames = nextTerritoryName :: currPlayer.territoryNames
      nextTerritory.occupant = currPlayer
      nextContinent.occupantNames += currPlayer.name
      unoccupiedTerritories = unoccupiedTerritories.tail
      i = i + 1
    }

    for (p <- players) {
      p.updateNeighbors()
    }

    //evenly distribute armies across all occupied territories
    var checkTotal: Int = 0
    for (p: Player <- players) {
      var checkMath: Int = 0
      p.armiesOnReserve = p.armiesOnReserve - 1  //subtract off the armies we place to claim each territory
      val div: Int = p.armiesOnReserve / p.getNumTerritories()
      val rem: Int = p.armiesOnReserve % p.getNumTerritories()
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

  def gameInProgress: Boolean = {
    var ret = true
    for (player <- players) {
      if (player.getNumTerritories() == GameMap.territoryMap.size) {
        ret = false
      }
    }
    ret
  }

  def end(): Unit = {
    state = End
  }

  def nextTurn(): Unit = {
    currentTurn = (currentTurn + 1) % players.length
    allocateArmies()
  }

  def getCurrentPlayer(): Player = players(getCurrentIndex())

  def getCurrentIndex(): Int = currentTurn % players.length

  def getCurrentAction(): Int = getCurrentPlayer().currentAction

  def allocateArmies(): Unit = getCurrentPlayer().allocateTurnAllotment()

  def removeLosingPlayers(): Unit = {
    players = players.filter(_.getNumTerritories() > 0)
  }

  def showCurrentAction(): String = state match {
    case Place => "place armies"
    case Attack => "attack enemy territories"
    case Defend => s"DEFEND: ${players(attacker.playerIndex).name} is attacking your ${attacker.attackTo} with ${attacker.numAttackers} armies."
    case Roll => "Battling!"
    case Fortify => "fortify your territories"
    case End => "game over"
  }

  def getPlayers(): List[Player] = players

  override def toString: String = {
    (for (i <- players) yield {
      i.toString()
    }).toString()
  }
}
