package controllers

import javax.inject.Inject

import models.Widget
import play.api.data._
import play.api.i18n._
import play.api.mvc._

/**
 * The classic WidgetController using MessagesAbstractController.
 *
 * Instead of MessagesAbstractController, you can use the I18nSupport trait,
 * which provides implicits that create a Messages instance from a request
 * using implicit conversion.
 *
 * See https://www.playframework.com/documentation/2.6.x/ScalaForms#passing-messagesprovider-to-form-helpers
 * for details.
 */
class WidgetController @Inject()(cc: MessagesControllerComponents) extends MessagesAbstractController(cc) {
  import WidgetForm._

  private val widgets = scala.collection.mutable.ArrayBuffer(Widget(""))
  private var playerList = List[String]()
  private var ipList = List[String]()
  private var playerCount = 0
  private var gameIsStarted = false

  // The URL to the widget.  You can call this directly from the template, but it
  // can be more convenient to leave the template completely stateless i.e. all
  // of the "WidgetController" references are inside the .scala file.
  private val postUrl = routes.WidgetController.createWidget()

  def index: Action[AnyContent] = Action {
    Ok(views.html.index())
  }

  // Corresponds to reset button to clear the player list.
  def reset: Action[AnyContent] = Action {
    if (playerCount == 0) {
      Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Cannot reset empty list")
    } else {
      widgets.clear()
      playerList = List[String]()
      playerCount = 0
      Redirect(routes.WidgetController.listWidgets()).flashing("Success" -> " Player list reset!")
    }
  }

  def join: Action[AnyContent] = Action {
    if (!gameIsStarted) {
      Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Game has not started yet")
    } else {
      Redirect(routes.GameController.show())
    }
  }

  // Corresponds to start game button to begin the game. Checks for min player amount and if satisfied creates a comma-
  // separated string of the player names to pass to the game controller.
  def start: Action[AnyContent] = Action {
    if (!gameIsStarted) {
      if (playerCount < 3) {
        Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Must have at least 3 players")
      } else {
        gameIsStarted = true
        Redirect(routes.GameController.startGame(playerList.mkString(",") + "-" + ipList.mkString(",")))
      }
    } else {
      Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Game is already in progress")
    }
  }

  def listWidgets: Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    // Pass an unpopulated form to the template
    Ok(views.html.listWidgets(widgets, form, postUrl))
  }

  // This will be the action that handles our form post
  def createWidget: Action[AnyContent] = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[Data] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.listWidgets(widgets, formWithErrors, postUrl))
    }

    val successFunction = { data: Data =>
      // This is the good case, where the form was successfully parsed as a Data object.
      val widget = Widget(name = data.name)
      if (!gameIsStarted){
        if (playerCount >= 6) {
          Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Cannot have more than 6 players")
        } else if (playerList.contains(widget.name)) {
          Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Cannot have duplicate names")
        } else if (widget.name.contains(",")) {
          Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Invalid character in name: comma")
        } else {
          ipList = request.remoteAddress :: ipList
          widgets.append(widget)
          playerList = widget.name :: playerList
          playerCount += 1
          Redirect(routes.WidgetController.listWidgets()).flashing("Success" -> " Player added!")
        }
      } else {
        Redirect(routes.WidgetController.listWidgets()).flashing("Success" -> " Game already started, cannot add players!")
      }
    }

    val formValidationResult = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
