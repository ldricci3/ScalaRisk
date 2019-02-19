package models

class Player(val name: String, val armies: Set[Army]) {

}

object Player {
  def addPlayer(name: String, armies: Set[Army]) = {
    new Player(name, armies)
  }
}