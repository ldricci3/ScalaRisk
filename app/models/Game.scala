package models

class Game(val names:  Set[String]) {
  require(names.size >= 3 && names.size <= 6, "Must have from 3 to 6 players")
  val gameMap: GameMap = new GameMap

  val numArmies = 50 - 5*names.size
  var baseArmy: Set[Army] = Set()
  for (i <- 1 to numArmies) {
    baseArmy += new Infantry;
  }
  val players = for (i <- names) yield {
    Player.addPlayer(i, baseArmy);
  }


}
