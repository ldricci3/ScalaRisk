package controllers

import javax.inject.Inject
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import scala.collection.mutable.ArrayBuffer
import play.api.libs.json._

import scala.collection.mutable.ListBuffer
import models.{BattleInfo, GameState}

case class InputText (input: String)

class GameController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  private val inputTextHistory = ArrayBuffer[InputText]()
  private val postUrl = routes.GameController.submit()
  private var submissionMessage = ""
  var game: models.Game = new models.Game
  val checker: CommandChecker = CommandChecker()

  val form: Form[InputText] = Form (
    mapping(
      "INPUT" -> nonEmptyText
    )(InputText.apply)(InputText.unapply)
  )

  def show: Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    // pass an unpopulated form to the template
    Ok(views.html.game(game, form, postUrl))
  }

  def showMobile: Action[AnyContent] = Action {implicit request: MessagesRequest[AnyContent] =>
    // show map portion only for mobile app
    Ok(views.html.mobile(game))
  }

  def submitMobile(gameCommand: String): Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    var cleaned_command = gameCommand.replaceAll("_", " ")
    tryCommand(cleaned_command)
    Ok(views.html.mobile(game))
  }

  def json: Action[AnyContent] = Action {
    val jsonValue: JsValue = JsObject(Seq(
      "Current Player" -> JsString(jsonGameStats()(0)),
      "Player Color" -> JsString(jsonGameStats()(1)),
      "Current Action" -> JsString(jsonGameStats()(2)),
      "Player Territories" -> JsArray(jsonPlayerTerritories()),
      "Attacking Dice" -> JsString(jsonGameStats()(3)),
      "Defending Dice" -> JsString(jsonGameStats()(4)),
      "Submission Message" -> JsString(submissionMessage)))
    Ok(jsonValue)
  }

  def jsonGameStats(): Array[String] = {
    var currentPlayer = ""
    var currentColor = ""
    var attackingDice = ""
    var defendingDice = ""
    if (game.state == models.Defend) {
      currentPlayer = models.GameMap.territoryMap(game.attacker.attackTo).occupant.name
      currentColor = models.GameMap.territoryMap(game.attacker.attackTo).occupant.color
    } else if (game.state == models.Roll){
      currentPlayer = models.GameMap.territoryMap(game.attacker.attackTo).occupant.name
      currentColor = models.GameMap.territoryMap(game.attacker.attackTo).occupant.color
      attackingDice = BattleInfo.attackRolls.mkString(",")
      defendingDice = BattleInfo.defendRolls.mkString(",")
    } else {
      currentPlayer = game.getCurrentPlayer().name
      currentColor = game.getCurrentPlayer().color
    }
    Array(currentPlayer, currentColor, game.showCurrentAction(), attackingDice, defendingDice)
  }

  def jsonPlayerTerritories(): Array[JsValue] = {
    val territories = game.getCurrentPlayer().territoryNames
    val jsvalueList = new ListBuffer[JsValue]()
    for (territory <- territories) {
      jsvalueList += JsString(territory)
    }
    jsvalueList.toArray
  }

  def submit: Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[InputText] =>
      // this is the bad case, where the form had validation errors.
      BadRequest(views.html.game(game, formWithErrors, postUrl))
    }

    val successFunction = { data: InputText =>
      // this is the SUCCESS case
      if (game.state == models.Defend) {
        if (models.GameMap.getTerritoryByName(game.attacker.attackTo).occupant.ip == request.remoteAddress) {
          val inputText = InputText(data.input)
          inputTextHistory.append(inputText)
          tryCommand(inputText.input)
          showMessage(submissionMessage)
        } else {
          submissionMessage = "You are not defending"
          checker.message = "You are not defending"
          showMessage(submissionMessage)
        }
      } else if (game.getCurrentPlayer().ip != request.remoteAddress) {
        submissionMessage = "It is not your turn"
        checker.message = "It is not your turn"
        showMessage(submissionMessage)
      } else {
        val inputText = InputText(data.input)
        inputTextHistory.append(inputText)
        tryCommand(inputText.input)
        showMessage(submissionMessage)
      }
    }

    val formValidationResult: Form[InputText] = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }

  def tryCommand(entireCmd: String): Unit = {
    val tokens: List[String] = entireCmd.split('(').toList
    val cmd: String = tokens.head.toLowerCase()
    val cmdChecks: String = checker.checkCommand(cmd, game)
    val temp: String = tokens.tail.mkString("")
    val params: Array[String] = temp.split(", ")
    if (cmdChecks != "passed") {
      saveMessage(cmdChecks)
    } else if (cmd == "next" && entireCmd != "next") {
      saveMessage("if you're moving onto next turn, only type: next")
    } else if (cmd != "defend" && cmd != "next" && params.length == 1) {
      saveMessage("params must be included within () and separated by [comma][space]")
    } else {
      tryCommandContinued(cmd, cmdChecks, params)
    }
  }

  def tryCommandContinued(cmd: String, cmdChecks: String, params: Array[String]): Unit = {
    val lastElem: String = params(params.length - 1)
    params(params.length - 1) = lastElem.split("\\)").toList.head

    val paramsChecks: String = checker.checkParams(cmd, params, game)
    if (cmdChecks == "passed" && paramsChecks == "passed") {
      runCommand(cmd, params)
    } else if (cmdChecks != "passed") {
      saveMessage(cmdChecks)
    } else if (paramsChecks != "passed") {
      saveMessage(paramsChecks)
    }
  }

  def runCommand(cmd: String, params: Array[String]): Unit = {
    cmd match {
      case "place" => place(params)
      case "attack" => prepareAttack(params)
      case "defend" => battle(params)
      case "fortify" => fortify(params)
      case "next" => next()
    }
  }

  def place(params: Array[String]): Unit = {
    game.getCurrentPlayer().placeArmies(params(0), params(1).toInt)
  }

  def prepareAttack(params: Array[String]): Unit = {
    game.attacker = models.Attacker(game.getCurrentIndex(), params(0), params(1), params(2).toInt)
    game.state = models.Defend
  }

  def battle(params: Array[String]): Unit = {
    val numDefenders: Int = params(0).toInt
    val attackerWins = game.getCurrentPlayer().attack(
      game.attacker.attackTo,
      game.attacker.attackFrom,
      game.attacker.numAttackers,
      numDefenders
    )
    if (attackerWins && !game.gameInProgress) {
      game.end()
    } else {
      next()
    }
  }

  def fortify(params: Array[String]): Unit = {
    game.getCurrentPlayer().fortify(params(0), params(1), params(2).toInt)
  }

  def next(): Unit = {
    game.nextState()
    saveMessage(s"Allowed commands: " + checker.allowedCommands(game).mkString("[", ", ", "]"))
  }

  // Gets comma-separated string of names and breaks them into a list, then instantiates the game
  def startGame(playerInfo: String): Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val infoArray = playerInfo.split("-")
    val playerNames = infoArray(0).split(",").toList
    val playerIPS = infoArray(1).split(",").toList

    val c = models.GameMap.getClass.getDeclaredConstructor()
    c.setAccessible(true)
    c.newInstance()
    game.setupGame(playerNames, List("Red", "White", "Yellow", "Green", "Blue", "Orange"), playerIPS)

    showMessage("");
  }
  def saveMessage(m: String): Unit = {
    checker.messageOn = false
    submissionMessage = m
  }
  def showMessage(message: String): Result =
    if (checker.messageOn) {
      Redirect(
        routes.GameController.show()).flashing("Message" -> checker.message)
    } else {
      Redirect(
        routes.GameController.show()).flashing("Message" -> message)
    }
}
