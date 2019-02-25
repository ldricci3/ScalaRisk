package models
import scala.util.Random
object Dice {
  val random: Random = new Random(System.currentTimeMillis)

  /**
    * Simulate rolling a single die.
    * @return the outcome represented as an Int from 1 to 6 inclusive
    */
  def rollOnce(): Int = {
    roll(1).head
  }

  /**
    * Simulate rolling a single die multiple times
    * @param repeat number of rolls
    * @return each outcome as a sorted IndexedSeq[Int]
    */
  def roll(repeat: Int): IndexedSeq[Int] = (for (i <- 0 until repeat) yield {
    random.nextInt(6) + 1
  }).sorted
}
