package models

class Game(val names:  Set[String]) {
  val players = List[Player]

  def initialize() ={
    for (i <- names) yield {

    }
  }
}

object Game {
  def newGame(names: Set[String]) = {
    val numArmies = 50 - 5*names.size
    var baseArmy: Set[Army] = Set()
    for (i <- 1 to numArmies) {
      baseArmy += new Infantry;
    }
    val players = for (i <- names) yield {
      Player.addPlayer(i, baseArmy);
    }
  }
}