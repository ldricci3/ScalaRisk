package controllers

import javax.inject.Inject
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import scala.collection.mutable.ArrayBuffer

case class InputText (input: String)

class GameController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {

  private val inputTextHistory = ArrayBuffer[InputText]()
  private val postUrl = routes.GameController.submit()
  private var submissionMessage = ""
  val game: models.Game = new models.Game()

  val form: Form[InputText] = Form (
    mapping(
      "input" -> nonEmptyText,
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
      checkCommand(inputText.input)
      showMessage(submissionMessage)
    }
    val formValidationResult: Form[InputText] = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
  
  def checkCommand(entireCmd: String): Unit = {
    val tokens: List[String] = entireCmd.split('(').toList
    val cmd: String = tokens.head
    val cmdChecks: String = checkCmd(cmd)

    val temp: String = tokens.tail.mkString("")
    val params: Array[String] = temp.split(", ")
    if (cmdChecks != "passed") {
      saveMessage(cmdChecks)
    } else if (cmd == "next" && entireCmd != "next") {
      saveMessage("if you're moving onto next turn, only type: next")
    } else if (cmd != "next" && params.length == 1) {
      saveMessage("params must be included within () and separated by [comma][space]")
    } else {
      val lastElem: String = params(params.length - 1)
      params(params.length - 1) = lastElem.split("\\)").toList.head

      val paramsChecks: String = checkParams(cmd, params)
      if (cmdChecks == "passed" && paramsChecks == "passed") {
        runCommand(cmd, params)
      } else if (cmdChecks != "passed") {
        saveMessage(cmdChecks)
      } else if (paramsChecks != "passed") {
        saveMessage(paramsChecks)
      }
    }
  }
  def checkCmd(cmd: String): String = {
    if (invalidCommand(cmd)) {
      s"$cmd is an invalid command."
    } else if (cmd != "place" && game.getCurrentPlayer().armiesOnReserve != 0) {
      "must place all armies before moving on."
    } else {
      "passed"
    }
  }
  private def invalidCommand(str: String): Boolean = str match {
    case "place" => false
    case "attack" => false
    case "fortify" => false
    case "next" => false
    case _ => true
  }
  private def checkParams(cmd: String, params: Array[String]): String = cmd match {
    case "place" => checkPlace(params)
    case "attack" => "fill in later"
    case "fortify" => "fill in later"
    case "next" => "passed"
  }
  private def checkPlace(params: Array[String]): String = {
    if (params.length != 2) {
      s"incorrect number of params. Expected 2."
    } else if (!playerOwnsTerritory(params(0))) {
      s"player does not own ${params(0)}"
    } else if (insufficientArmies(params(1).toInt)) {
      s"insufficient armies in reserve. Actual: ${params(1)}."
    } else if (negativeNumArmies(params(1).toInt)) {
      s"cannot place negative armies. Actual: ${params(1)}."
    } else {
      saveMessage(s"successfully placed  ${params(1)} armies in  ${params(0)}")
      "passed"
    }
  }
  private def playerOwnsTerritory(name: String): Boolean =
    game.getCurrentPlayer().territoryNames.contains(name)

  private def insufficientArmies(n: Int): Boolean = n > game.getCurrentPlayer().armiesOnReserve
  private def negativeNumArmies(n: Int): Boolean = n < 0

  def runCommand(cmd: String, params: Array[String]): Unit = cmd match {
    case "place" => place(params)
    case "attack" => game.getCurrentPlayer().attack()
    case "fortify" => game.getCurrentPlayer().fortify()
    case "next" => next()
  }

  def place(params: Array[String]): Unit = {
    game.getCurrentPlayer().placeArmies(params(0), params(1).toInt)
  }

  def attack(): Unit = {

  }

  def fortify(): Unit = {

  }

  def next(): Unit = {
    game.next()
    game.getCurrentPlayer().allocateTurnAllotment()
    saveMessage(s"successfully moved on to ${game.getCurrentPlayer().name}'s turn.")
  }

  // Gets comma-separated string of names and breaks them into a list, then instantiates the game
  def startGame(playerNames: String) = Action { implicit request: MessagesRequest[AnyContent] =>
    if (!game.isStarted) {
      val playerArray = playerNames.split(",")
      val playerList = playerArray.toList

      val c = models.GameMap.getClass.getDeclaredConstructor()
      c.setAccessible(true)
      c.newInstance()
      game.setupGame(playerList, List("Red", "White", "Yellow", "Green", "Blue", "Orange"))
    }

    Ok(views.html.game(game, form, postUrl))
  }
  def saveMessage(m: String): Unit = submissionMessage = m
  def showMessage(message: String): Result = Redirect(
    routes.GameController.show()).flashing("Message" -> message)
}
