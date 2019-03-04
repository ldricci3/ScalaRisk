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
  private var playerCount = 0

  // The URL to the widget.  You can call this directly from the template, but it
  // can be more convenient to leave the template completely stateless i.e. all
  // of the "WidgetController" references are inside the .scala file.
  private val postUrl = routes.WidgetController.createWidget()

  def index = Action {
    Ok(views.html.index())
  }

  // Corresponds to reset button to clear the player list.
  def reset = Action {
    if (playerCount == 0) {
      Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Cannot reset empty list")
    } else {
      widgets.clear()
      playerList = List[String]()
      playerCount = 0
      Redirect(routes.WidgetController.listWidgets()).flashing("Success" -> " Player list reset!")
    }
  }

  // Corresponds to start game button to begin the game. Checks for min player amount and if satisfied creates an
  // instance of game and redirects to the game page.
  def start = Action {
    if (playerCount < 3) {
      Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Must have at least 3 players")
    } else {
      val game: models.Game = new models.Game(playerList, List[String]());
      Redirect(routes.GameController.game())
    }
  }

  def listWidgets = Action { implicit request: MessagesRequest[AnyContent] =>
    // Pass an unpopulated form to the template
    Ok(views.html.listWidgets(widgets, form, postUrl))
  }

  // This will be the action that handles our form post
  def createWidget = Action { implicit request: MessagesRequest[AnyContent] =>
    val errorFunction = { formWithErrors: Form[Data] =>
      // This is the bad case, where the form had validation errors.
      // Let's show the user the form again, with the errors highlighted.
      // Note how we pass the form with errors to the template.
      BadRequest(views.html.listWidgets(widgets, formWithErrors, postUrl))
    }

    val successFunction = { data: Data =>
      // This is the good case, where the form was successfully parsed as a Data object.
      val widget = Widget(name = data.name)
      // This checks for duplicate names(case-sensitive), then adds the player if not a duplicate and count < 6
      if (playerCount >= 6) {
        Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Cannot have more than 6 players")
      } else if (playerList.contains(widget.name)) {
        Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Cannot have duplicate names")
      } else {
        widgets.append(widget)
        playerList = widget.name :: playerList
        playerCount += 1
        Redirect(routes.WidgetController.listWidgets()).flashing("Success" -> " Player added!")
      }
    }

    val formValidationResult = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
