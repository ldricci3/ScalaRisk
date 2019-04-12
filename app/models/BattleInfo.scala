package models

case object BattleInfo {
  var playerIndex: Int = 0
  var attackTo: String = ""
  var attackFrom: String = ""
  var numAttackers: Int = 0
  var attackRolls: List[Int] = List.empty[Int]
  var defendRolls: List[Int] = List.empty[Int]
}
