package models
import scala.util.Random
trait hasDice {
  val random: Random = new Random(System.currentTimeMillis)

  def rollOnce(): Int = {
    roll(1).head
  }
  def roll(repeat: Int): IndexedSeq[Int] = for (i <- 0 until repeat) yield {
    random.nextInt(6) + 1
  }
}
