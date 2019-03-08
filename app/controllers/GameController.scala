package controllers

import javax.inject._
import play.api.mvc._

class GameController @Inject()(cc: ControllerComponents)(implicit assetsFinder: AssetsFinder)
  extends AbstractController(cc) {

  // Gets comma-separated string of names and breaks them into a list, then instantiates the game
  def startGame(playerNames: String) = Action {
    val playerArray = playerNames.split(",")
    val playerList = playerArray.toList

    val c = models.GameMap.getClass.getDeclaredConstructor()
    c.setAccessible(true)
    c.newInstance()
    val game: models.Game = new models.Game(playerList, List("Red", "White", "Yellow", "Green", "Blue", "Orange"))

    Ok(views.html.game(game))
  }
}
