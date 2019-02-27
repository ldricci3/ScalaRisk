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

  private val widgets = scala.collection.mutable.ArrayBuffer(Widget(null))
  var playerList = List[String]()
  var playerCount = 0

  // The URL to the widget.  You can call this directly from the template, but it
  // can be more convenient to leave the template completely stateless i.e. all
  // of the "WidgetController" references are inside the .scala file.
  private val postUrl = routes.WidgetController.createWidget()

  def index = Action {
    Ok(views.html.index())
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
      if (widget.name.toLowerCase() == "start") {
        // This checks if the user submitted the start command, and then checks if the number of players is valid
        if (playerCount < 3) {
          Redirect(routes.WidgetController.listWidgets()).flashing("Error" -> " Must have at least 3 players")
        } else {
          println("Starting Game") //Just here to show in sbt shell if user calling start was successful
          //Call Game with playerList here
          //Can attempt to redirect to game HTML later on
          Redirect(routes.WidgetController.listWidgets()).flashing("Game Starting" -> " Please wait")
        }
      } else if (widget.name.toLowerCase() == "reset") {
        // This checks if the user submitted the reset command, the resets all vars and the table of player names
        widgets.clear()
        playerList = List[String]()
        playerCount = 0
        Redirect(routes.WidgetController.listWidgets()).flashing("Success" -> " Player list reset!")
      } else {
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
    }

    val formValidationResult = form.bindFromRequest
    formValidationResult.fold(errorFunction, successFunction)
  }
}
