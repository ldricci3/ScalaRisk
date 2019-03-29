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
      println(inputText)
      Redirect(routes.GameController.show()).flashing("Success" -> "Input taken!")
    }

    val formValidationResult: Form[InputText] = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
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
}
