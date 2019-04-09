package controllers

import javax.inject.Inject
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import scala.collection.mutable.ArrayBuffer

import models.GameState

case class InputText (input: String)

class GameController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  private val inputTextHistory = ArrayBuffer[InputText]()
  private val postUrl = routes.GameController.submit()
  private var submissionMessage = ""
  var game: models.Game = new models.Game

  val form: Form[InputText] = Form (
    mapping(
      "INPUT" -> nonEmptyText
    )(InputText.apply)(InputText.unapply)
  )

  def show = Action { implicit request: MessagesRequest[AnyContent] =>
    // pass an unpopulated form to the template
    Ok(views.html.game(game, form, postUrl))
  }

  def submit = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[InputText] =>
      // this is the bad case, where the form had validation errors.
      BadRequest(views.html.game(game, formWithErrors, postUrl))
    }

    val successFunction = { data: InputText =>
      // this is the SUCCESS case
      val inputText = InputText(data.input)
      inputTextHistory.append(inputText)
      tryCommand(inputText.input)
      showMessage(submissionMessage)
    }
    val formValidationResult: Form[InputText] = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def tryCommand(entireCmd: String): Unit = {
    val tokens: List[String] = entireCmd.split('(').toList
    val cmd: String = tokens.head.toLowerCase()
    val cmdChecks: String = checkCommand(cmd)

    val temp: String = tokens.tail.mkString("")
    val params: Array[String] = temp.split(", ")
    if (cmdChecks != "passed") {
      saveMessage(cmdChecks)
    } else if (cmd == "next" && entireCmd != "next") {
      saveMessage("if you're moving onto next turn, only type: next")
    } else if (cmd != "defend" && cmd != "next" && params.length == 1) {
      saveMessage("params must be included within () and separated by [comma][space]")
    } else {
      val lastElem: String = params(params.length - 1)
      params(params.length - 1) = lastElem.split("\\)").toList.head

      val paramsChecks: String = checkParams(cmd, params)
      if (cmdChecks == "passed" && paramsChecks == "passed") {
        println(s"running command $cmd")
        runCommand(cmd, params)
        println(s"finished command $cmd")
      } else if (cmdChecks != "passed") {
        saveMessage(cmdChecks)
      } else if (paramsChecks != "passed") {
        saveMessage(paramsChecks)
      }
    }
  }
  def allowedCommands(): Set[String] = game.state match {
    case models.Place => Set("next", "place")
    case models.Attack => Set("next", "attack")
    case models.Defend => Set("next", "defend")
    case models.Fortify => Set("next", "fortify")
    case models.Roll => Set("next")
  }
  def isAllowedCommand(cmd: String): (Boolean, Set[String]) = (allowedCommands().contains(cmd), allowedCommands())
  def checkCommand(cmd: String): String = {
    var ret: String = "passed"
    if (invalidCommand(cmd)) {
      ret = s"$cmd is an invalid command."
    } else {
      val (isAllowed, supportedCommands) = isAllowedCommand(cmd)
      if (!isAllowed) {
        ret = s"$cmd is not allowed for current action. Allowed commands: " + supportedCommands.mkString("[", ", ", "]")
      } else {
        //TODO check conditions specific to state
        ret = game.state match {
          case models.Place => checkCommandPlace(cmd)
          case models.Attack => checkCommandAttack(cmd)
          case models.Defend => checkCommandDefend(cmd)
          case models.Fortify => checkCommandFortify(cmd)
          case _ => "passed"
        }
      }
    }
    ret
  }
  private def checkCommandPlace(cmd: String): String =  {
    var ret = "passed"
    if (cmd == "next" && game.getCurrentPlayer().armiesOnReserve != 0) {
      ret = s"cannot move on to next action until all remaining ${game.getCurrentPlayer().armiesOnReserve} armies are placed."
    }
    ret
  }
  private def checkCommandAttack(cmd: String): String = {
    var ret = "passed"
    //TODO
    if (cmd == "next") {
      ""
    }
    ret
  }
  private def checkCommandDefend(cmd: String): String = {
    var ret = "passed"
    //TODO
    if (cmd == "next") {
      ""
    }
    ret
  }
  private def checkCommandFortify(cmd: String): String = {
    var ret = "passed"
    //TODO final milestone.
    if (cmd == "next") {
      ""
    }
    ret
  }
  private def invalidCommand(str: String): Boolean = str match {
    case "place" => false
    case "attack" => false
    case "defend" => false
    case "fortify" => false
    case "next" => false
    case _ => true
  }
  private def checkParams(cmd: String, params: Array[String]): String = cmd match {
    case "place" => checkPlace(params)
    case "attack" => checkAttack(params)
    case "defend" => checkDefend(params)
    case "fortify" => "fill in later"
    case "next" => "passed"
  }
  private def checkPlace(params: Array[String]): String = {
    if (params.length != 2) {
      s"incorrect number of params. Expected 2."
    } else if (!playerOwnsTerritory(params(0))) {
      s"player does not own ${params(0)}"
    } else if (insufficientArmiesPlace(params(1).toInt)) {
      s"insufficient armies in reserve. Input: ${params(1)}. Available: ${game.getCurrentPlayer().armiesOnReserve}"
    } else if (negativeNumArmies(params(1).toInt)) {
      s"cannot place negative armies. Input: ${params(1)}. Available: ${game.getCurrentPlayer().armiesOnReserve}"
    } else {
      saveMessage(s"successfully placed  ${params(1)} armies in  ${params(0)}")
      "passed"
    }
  }

  private def checkAttack(params: Array[String]): String = {
    println("begin attack param checks")
    var ret: String = "passed"
    if (params.length != 3) {
      ret = "incorrect number of params. Expected 3. attack(\"neighbor\", \"myTerritory\", numArmiesToSend)"
    } else {
      val Array(attackHere, attackFrom, numArmiesToSend) = params
      println(numArmiesToSend.toInt)
      println(exceedsThreeDice(numArmiesToSend.toInt))
      ret = if (!playerOwnsTerritory(attackFrom)) {
        s"${game.getCurrentPlayer()} does not own $attackFrom"
      } else if (!models.GameMap.adjacencySet(attackFrom).contains(attackHere)) {
        s"$attackFrom and $attackHere are not neighboring territories"
      } else if (playerOwnsTerritory(attackHere)) {
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
        println("attack command checks passed")
        "passed"
      }
    }
    println("finish attack param checks")
    ret
  }
  private def exceedsThreeDice(n: Int): Boolean = n > 3
  private def checkDefend(params: Array[String]): String = {
    var ret: String = "passed"
    var numDefenders: Int = params(0).toInt
    if (params.length != 1) {
      ret = "incorrect number of params. Expected 1. defend(numDefendersofTerritory)"
    } else {
      val models.Attacker(attackerIndex, attackHere, attackFrom, numArmiesToSend) = game.attacker
      if (negativeNumArmies(numDefenders)) {
        ret = s"cannot defend with negative armies. Input: $numDefenders. Available: ${models.GameMap.territoryMap(game.attacker.attackTo).numArmies}"
      } else if (insufficientArmiesDefend(numDefenders, game.attacker.attackTo)) {
        s"insufficient armies in $attackHere. Input: $numDefenders. Available: ${models.GameMap.territoryMap(attackHere).numArmies}"
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
  private def playerOwnsTerritory(name: String): Boolean =
    game.getCurrentPlayer().ownsTerritory(name)

  private def insufficientArmiesPlace(n: Int): Boolean = n > game.getCurrentPlayer().armiesOnReserve
  private def insufficientArmiesAttack(n: Int, attackFrom: String): Boolean = n >= models.GameMap.territoryMap(attackFrom).numArmies
  private def insufficientArmiesDefend(n: Int, defendHere: String): Boolean = n > models.GameMap.territoryMap(defendHere).numArmies
  private def insufficientArmiesFortify(n: Int): Boolean = n > game.getCurrentPlayer().armiesOnReserve

  private def negativeNumArmies(n: Int): Boolean = n < 0

  def runCommand(cmd: String, params: Array[String]): Unit = cmd match {
    case "place" => place(params)
    case "attack" => prepareAttack(params)
    case "defend" => battle(params)
    case "fortify" => fortify()
    case "next" => next()
  }

  def place(params: Array[String]): Unit = {
    game.getCurrentPlayer().placeArmies(params(0), params(1).toInt)
  }

  def prepareAttack(params: Array[String]): Unit = {
    game.attacker = models.Attacker(game.getCurrentIndex(), params(0), params(1), params(2).toInt)
    println("attack prepared.")
    game.state = models.Defend
  }

  def battle(params: Array[String]): Unit = {
    val numDefenders: Int = params(0).toInt
    game.getCurrentPlayer().attack(
      game.attacker.attackTo,
      game.attacker.attackFrom,
      game.attacker.numAttackers,
      numDefenders
    )
    next()
  }

  def fortify(): Unit = {
    game.getCurrentPlayer.fortify()
  }

  def next(): Unit = {
    game.nextState()
    saveMessage(s"Allowed commands: " + allowedCommands().mkString("[", ", ", "]"))
  }
  /*def nextAction(): Unit = {
    //game.nextAction()
    saveMessage(s"successfully moved on to action: ${game.showCurrentAction()}.")
  }
  def nextTurn(): Unit = {
    //game.nextAction()
    //game.nextTurn()
    game.getCurrentPlayer().allocateTurnAllotment()
    saveMessage(s"successfully moved on to ${game.getCurrentPlayer().name}'s turn.")
  }*/

  // Gets comma-separated string of names and breaks them into a list, then instantiates the game
  def startGame(playerNames: String) = Action { implicit request: MessagesRequest[AnyContent] =>
    if (!game.isStarted) {
      val playerArray = playerNames.split(",")
      val playerList = playerArray.toList

      /*val c = models.GameMap.getClass.getDeclaredConstructor()
      c.setAccessible(true)
      c.newInstance()*/
      game.setupGame(playerList, List("Red", "White", "Yellow", "Green", "Blue", "Orange"))
    }
    Ok(views.html.game(game, form, postUrl))
  }
  def saveMessage(m: String): Unit = submissionMessage = m
  def showMessage(message: String): Result = Redirect(
    routes.GameController.show()).flashing("Message" -> message)
}
