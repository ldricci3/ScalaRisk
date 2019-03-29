package controllers

import javax.inject._
import play.api.mvc._

class GameController @Inject()(cc: ControllerComponents)(implicit assetsFinder: AssetsFinder)
  extends AbstractController(cc) {

  val game: models.Game = new models.Game()

  // Gets comma-separated string of names and breaks them into a list, then instantiates the game
  def startGame(playerNames: String) = Action {
    if (!game.isStarted) {
      val playerArray = playerNames.split(",")
      val playerList = playerArray.toList

      val c = models.GameMap.getClass.getDeclaredConstructor()
      c.setAccessible(true)
      c.newInstance()
      game.setupGame(playerList, List("Red", "White", "Yellow", "Green", "Blue", "Orange"))
    }

    Ok(views.html.game(game))
  }

  def displayGame(game: models.Game) = Action {
    Ok(views.html.game(game))
  }

  // Handle user input form here

}
