package controllers

case class CommandChecker() {
  var message: String = ""
  var messageOn: Boolean = false
  def allowedCommands(game: models.Game): Set[String] = game.state match {
    case models.Place => Set("next", "place")
    case models.Attack => Set("next", "attack")
    case models.Defend => Set("next", "defend")
    case models.Fortify => Set("next", "fortify")
    case models.Roll => Set("next")
  }

  def isAllowedCommand(cmd: String, game: models.Game): (Boolean, Set[String]) = (allowedCommands(game).contains(cmd), allowedCommands(game))

  def checkCommand(cmd: String, game: models.Game): String = {
    var ret: String = "passed"
    if (invalidCommand(cmd)) {
      ret = s"$cmd is an invalid command."
    } else {
      val (isAllowed, supportedCommands) = isAllowedCommand(cmd, game)
      if (!isAllowed) {
        ret = s"$cmd is not allowed for current action. Allowed commands: " + supportedCommands.mkString("[", ", ", "]")
      } else {
        //TODO check conditions specific to state
        ret = game.state match {
          case models.Place => checkCommandPlace(cmd, game)
          case models.Attack => checkCommandAttack(cmd)
          case models.Defend => checkCommandDefend(cmd)
          case models.Fortify => checkCommandFortify(cmd)
          case _ => "passed"
        }
      }
    }
    ret
  }

  def checkCommandPlace(cmd: String, game: models.Game): String =  {
    var ret = "passed"
    if (cmd == "next" && game.getCurrentPlayer().armiesOnReserve != 0) {
      ret = s"cannot move on to next action until all remaining ${game.getCurrentPlayer().armiesOnReserve} armies are placed."
    }
    ret
  }

  def checkCommandAttack(cmd: String): String = {
    var ret = "passed"
    ret
  }

  def checkCommandDefend(cmd: String): String = {
    var ret = "passed"
    ret
  }

  def checkCommandFortify(cmd: String): String = {
    var ret = "passed"
    ret
  }

  def invalidCommand(str: String): Boolean = str match {
    case "place" => false
    case "attack" => false
    case "defend" => false
    case "fortify" => false
    case "next" => false
    case _ => true
  }

  def checkParams(cmd: String, params: Array[String], game: models.Game): String = cmd match {
    case "place" => checkPlace(params, game)
    case "attack" => checkAttack(params, game)
    case "defend" => checkDefend(params, game)
    case "fortify" => "fill in later"
    case "next" => "passed"
  }

  def checkPlace(params: Array[String], game: models.Game): String = {
    if (params.length != 2) {
      s"incorrect number of params. Expected 2."
    } else if (!playerOwnsTerritory(params(0), game)) {
      s"player does not own ${params(0)}"
    } else if (insufficientArmiesPlace(params(1).toInt, game)) {
      s"insufficient armies in reserve. Input: ${params(1)}. Available: ${game.getCurrentPlayer().armiesOnReserve}"
    } else if (negativeNumArmies(params(1).toInt)) {
      s"cannot place negative armies. Input: ${params(1)}. Available: ${game.getCurrentPlayer().armiesOnReserve}"
    } else {
      saveMessage(s"successfully placed  ${params(1)} armies in  ${params(0)}")
      "passed"
    }
  }

  private def checkAttack(params: Array[String], game: models.Game): String = {
    var ret: String = "passed"
    if (params.length != 3) {
      ret = "incorrect number of params. Expected 3. attack(\"neighbor\", \"myTerritory\", numArmiesToSend)"
    } else {
      val Array(attackHere, attackFrom, numArmiesToSend) = params
      ret = if (!playerOwnsTerritory(attackFrom, game)) {
        s"${game.getCurrentPlayer().name} does not own $attackFrom"
      } else if (!models.GameMap.adjacencySet(attackFrom).contains(attackHere)) {
        s"$attackFrom and $attackHere are not neighboring territories"
      } else if (playerOwnsTerritory(attackHere, game)) {
        s"${game.getCurrentPlayer()} owns $attackHere. cannot attack it."
      } else if (insufficientArmiesAttack(numArmiesToSend.toInt, attackFrom)) {
        s"insufficient armies in $attackFrom. Input: $numArmiesToSend. Available: ${models.GameMap.territoryMap(attackFrom).numArmies - 1}"
      } else if (exceedsThreeDice(numArmiesToSend.toInt)) {
        s"cannot roll more than 3 dice."
      }else if (negativeNumArmies(numArmiesToSend.toInt)) {
        s"cannot attack with negative armies. Input: $numArmiesToSend. Available: ${models.GameMap.territoryMap(attackFrom).numArmies - 1}"
      } else if (notOneTwoThree(numArmiesToSend.toInt)) {
        s"at the moment, c" +
          s"an only send 1, 2, or 3 armies. we will add support for > 3 armies later."
      } else {
        saveMessage("territory under siege!!!")
        "passed"
      }
    }
    ret
  }

  private def exceedsThreeDice(n: Int): Boolean = n > 3

  private def exceedsTwoDice(n: Int): Boolean = n > 2

  private def checkDefend(params: Array[String], game: models.Game): String = {
    var ret: String = "passed"
    var numDefenders: Int = params(0).toInt
    if (params.length != 1) {
      ret = "incorrect number of params. Expected 1. defend(numDefendersofTerritory)"
    } else {
      val models.Attacker(attackerIndex, attackHere, attackFrom, numArmiesToSend) = game.attacker
      if (negativeNumArmies(numDefenders)) {
        ret = s"cannot defend with negative armies. Input: $numDefenders. Available: ${models.GameMap.territoryMap(game.attacker.attackTo).numArmies}"
      } else if (insufficientArmiesDefend(numDefenders, game.attacker.attackTo)) {
        ret = s"insufficient armies in $attackHere. Input: $numDefenders. Available: ${models.GameMap.territoryMap(attackHere).numArmies}"
      } else if (exceedsTwoDice(numDefenders)) {
        ret = s"cannot defend with more than two armies."
      } else {
        saveMessage("Defending")
      }
    }
    ret
  }

  private def notOneTwoThree(n: Int): Boolean = n match {
    case 1 => false
    case 2 => false
    case 3 => false
    case _ => true
  }
  private def playerOwnsTerritory(name: String, game: models.Game): Boolean =
    game.getCurrentPlayer().ownsTerritory(name)

  private def insufficientArmiesPlace(n: Int, game: models.Game): Boolean = n > game.getCurrentPlayer().armiesOnReserve
  private def insufficientArmiesAttack(n: Int, attackFrom: String): Boolean = n >= models.GameMap.territoryMap(attackFrom).numArmies
  private def insufficientArmiesDefend(n: Int, defendHere: String): Boolean = n > models.GameMap.territoryMap(defendHere).numArmies
  private def insufficientArmiesFortify(n: Int, game: models.Game): Boolean = n > game.getCurrentPlayer().armiesOnReserve

  private def negativeNumArmies(n: Int): Boolean = n < 0


  private def saveMessage(str: String): Unit = {
    messageOn = true
    message = str
  }
}
