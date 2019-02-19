package models

class Game(val names:  List[String]) {
  require(names.size >= 3 && names.size <= 6, "Must have from 3 to 6 players")
  val gameMap: GameMap = new GameMap

  private var baseArmy: Set[Army] = Set()
  for (i <- 1 to (50 - 5*names.size)) {
    baseArmy += new Infantry;
  }

  val players = scala.util.Random.shuffle(for (i <- names) yield {
    new Player(i, baseArmy);
  })


}
