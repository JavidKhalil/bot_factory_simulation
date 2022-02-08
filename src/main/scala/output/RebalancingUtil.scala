package output

import output.BotProcessingFactory.Statement

import scala.collection.mutable
import scala.collection.mutable.Seq

object RebalancingUtil {
  /**
    * We need method for checking microchips map is there any
    * bot, which has chips with 17 and 61 number
    *
    * @param firstChip
    * @param secondChip
    * @return
    */
  def isThereBotWith17And61Chip(firstChip: Int, secondChip: Int): Boolean =
    Statement.microChips.
      filter(tuple => tuple._2.contains(firstChip) && tuple._2.contains(secondChip)).values.isEmpty

  /**
    * We need find bots with at least two chips
    * for starting rebalancing
    *
    * @return
    */
  def findTwoChipsInBot: mutable.Map[Int, mutable.Seq[Int]] = Statement.microChips.filter(tuple => tuple._2.size > 1)

  /**
    * After we find bots with at least two chips
    * We start rebalancing, so we need update microchips map for selected bots
    *
    * @param bots
    */
  def updateMicroChipMapForSelectedBots(bots: mutable.Map[Int, mutable.Seq[Int]]): Unit =
    for (bot <- bots) {
      for (bot1 <- Statement.botsStateMap.filter(tuple => tuple._1 == bot._1)) {
        Statement.microChips.get(bot1._2.heGivesHighToThis) match {
          case Some(value) =>
            Statement.microChips.put(bot1._2.heGivesHighToThis, value :+ bot._2.max)
          case None =>
            Statement.microChips.put(bot1._2.heGivesHighToThis, Seq(bot._2.max))
        }
        Statement.microChips.get(bot1._2.heGivesLowerToThis) match {
          case Some(value) =>
            Statement.microChips.put(bot1._2.heGivesLowerToThis, value :+ bot._2.min)
          case None =>
            Statement.microChips.put(bot1._2.heGivesLowerToThis, Seq(bot._2.min))
        }
      }
    }

  /**
    * Detects bot which is responsible for balancing required chips
    *
    * @param firstChip
    * @param secondChip
    * @return
    */
  def detectBotForDealWithChips(firstChip: Int, secondChip: Int): Int =
    Statement.microChips.filter(tuple => tuple._2.contains(firstChip) && tuple._2.contains(secondChip)).toList.head._1

  /**
    * Adds chip to map
    *
    * @param chipNumber
    */
  def addChipToMap(chipNumber: Int): Unit = {
    Statement.microChips.toList.foreach(tuple => {
      if (!tuple._2.contains(chipNumber)) {
        Statement.microChips.put(tuple._1, tuple._2 :+ chipNumber)
      }
    })
  }
}
