package controllers

import javax.inject._
import play.api.mvc._

class GameController @Inject()(cc: ControllerComponents)(implicit assetsFinder: AssetsFinder)
  extends AbstractController(cc) {

}
