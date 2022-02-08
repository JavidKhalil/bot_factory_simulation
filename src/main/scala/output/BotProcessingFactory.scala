package output

import org.slf4j.{Logger, LoggerFactory}
import output.Patterns._

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.collection.mutable._
import scala.collection.mutable.Map
import scala.collection.mutable.Map._
import scala.runtime.LazyLong
import scala.util.matching.Regex

object BotProcessingFactory {

  val log = LoggerFactory.getLogger("Main")

  /**
    * Generic Bot class
    *
    * @param botNumber
    * @param heGivesLowerToThis
    * @param heGivesHighToThis
    */
  case class BotGeneric(botNumber: Int, heGivesLowerToThis: Int, heGivesHighToThis: Int)

  /**
    * Bot class for case when we have template like
    * "bot N gives low to output N and high to bot N
    *
    * @param botNumber
    * @param heGivesLowerToThisOutput
    * @param heGivesHighToThis
    */
  case class BotOutputBot(botNumber: Int, heGivesLowerToThisOutput: Int, heGivesHighToThis: Int)

  /**
    * Bot class for case when we have template like
    * "bot N gives low to bot N and high to output N
    *
    * @param botNumber
    * @param heGivesLowerToThis
    * @param heGivesHighToThisOutput
    */
  case class BotBotOutput(botNumber: Int, heGivesLowerToThis: Int, heGivesHighToThisOutput: Int)

  /**
    * Bot class for case when we have template like
    * "bot N gives low to output N and high to output N
    *
    * @param botNumber
    * @param heGivesLowerToThisOutput
    * @param heGivesHighToThisOutput
    */
  case class BotOutputOutput(botNumber: Int, heGivesLowerToThisOutput: Int, heGivesHighToThisOutput: Int)

  /**
    * Class for parsing each of line of instruction
    *
    * @param fileName
    */
  case class Statement(fileName: String)

  case object Statement {

    /**
      * Maps, which incapsulate State of processing
      *
      */
    var botsStateMap: Map[Int, BotGeneric] = Map.empty
    var botOutputStateMap: Map[Int, BotGeneric] = Map.empty
    var outputBotStateMap: Map[Int, BotGeneric] = Map.empty
    var outputOutputStateMap: Map[Int, BotGeneric] = Map.empty
    var microChips: Map[Int, Seq[Int]] = Map.empty

    def apply(fileName: String) = {

      import Patterns._
      import Parsers._

      val content: BufferedSource = Source.fromFile(fileName, "UTF8")
      val listOfContents = content.getLines().toList

      for (line <- listOfContents) {

        /**
          * Store data to collections
        **/
        val botsNums: List[String] = regexpNums.findAllIn(line).toList
        val botsRes: Option[Regex.Match] = botGivesLowAndHighRegExp.findFirstMatchIn(line)
        val botsBotOutput: Option[Regex.Match] = regexpBotOutput.findFirstMatchIn(line)
        val botsOutputBot: Option[Regex.Match] = regexpOutputBot.findFirstMatchIn(line)
        val botsOutputOutput: Option[Regex.Match] = regexpOutputOutput.findFirstMatchIn(line)
        val valuesGoesReg: Option[Regex.Match] = valueGoes.findFirstMatchIn(line)

        /**
          * Parse instruction file to collect data to state maps
          *
          */
        parseBotVsBot(botsNums, line)
        parseBotOutputPattern(botsNums, line)
        parseOutputBotPattern(botsNums, line)
        parseOutputOutputPattern(botsNums, line)
        parseValuesPattern(botsNums, line)

      }
    }

  }

  def main(args: Array[String]): Unit = {

    import RebalancingUtil._

    val instructionFile = "input.txt"

    log.info(s"Start parsing instruction file: $instructionFile")

    Statement("input.txt")

    log.info("Starting calculation... ")

    //while there is no bot has 17 and 61 chips we should run in loop
    while (isThereBotWith17And61Chip(OutboxContantsUtil.firstChip, OutboxContantsUtil.secondChip)) {
      log.info("Microchips balancing current state: " + Statement.microChips)
      updateMicroChipMapForSelectedBots(findTwoChipsInBot)
    }

    log.info(s"The bot: ${detectBotForDealWithChips(OutboxContantsUtil.firstChip, OutboxContantsUtil.secondChip)} " +
      s"is responsible for comparing value-61 microchips with value-17 microchips")

    log.info(s"We multiply together the values of one chip in each of outputs 0, 1, and 2 will got " +
      s"${OutboxContantsUtil.seqRes.foldLeft(1)(_ * Statement.microChips.getOrElse(_, Seq(1)).head)}")

  }

  /**
    * encapsuletes logic for parsing lines from incstruction file
    *
    *
    * */
  object Parsers {

    import Statement.botsStateMap
    import Statement.outputOutputStateMap
    import Statement.outputBotStateMap
    import Statement.microChips
    import Statement.botOutputStateMap

    def parseBotOutputPattern(botsNums: List[String], line: String) = {
      regexpBotOutput.findFirstMatchIn(line) match {
        case Some(value) => {
          val bot1 = botsNums(0).toInt
          val output1 = botsNums(1).toInt
          val bot2 = botsNums(2).toInt
          botOutputStateMap.get(bot1) match {
            case Some(value) if value.heGivesLowerToThis < 0 => botOutputStateMap.put(bot1, BotGeneric(bot1, output1, bot2))
            case Some(value) if value.heGivesLowerToThis >= 0 => ()
            case None => botOutputStateMap.put(bot1, BotGeneric(bot1, output1, bot2))
          }
        }
        case None => ()
      }
    }

    def parseOutputBotPattern(botsNums: List[String], line: String) = {
      regexpOutputBot.findFirstMatchIn(line) match {
        case Some(value) =>
          val bot1 = botsNums(0).toInt
          val output1 = botsNums(1).toInt
          val bot2 = botsNums(2).toInt
          outputBotStateMap.get(bot1) match {
            case Some(value1) if value1.heGivesLowerToThis < 0 =>
              outputBotStateMap.put(bot1, BotGeneric(bot1, output1, bot2))
            case Some(value1) if value1.heGivesLowerToThis >= 0 =>
              ()
            case None =>
              val out = BotGeneric(bot1, output1, bot2)
              outputBotStateMap.put(bot1, out)
          }
        case None => ()
      }
    }

    def parseOutputOutputPattern(botsNums: List[String], line: String) = {
      regexpOutputOutput.findFirstMatchIn(line) match {
        case Some(value) =>
          val bot1 = botsNums(0).toInt
          val output1 = botsNums(1).toInt
          val bot2 = botsNums(2).toInt
          outputOutputStateMap.get(bot1) match {
            case Some(value1) if value1.heGivesLowerToThis < 0 =>
              outputOutputStateMap.put(bot1, BotGeneric(bot1, output1, bot2))
            case Some(value1) if value1.heGivesLowerToThis >= 0 =>
              ()
            case None =>
              val out = BotGeneric(bot1, output1, bot2)
              outputOutputStateMap.put(bot1, out)
          }
        case None => ()
      }
    }

    def parseValuesPattern(botsNums: List[String], line: String) = {
      valueGoes.findFirstMatchIn(line) match {
        case Some(value) =>
          val microChip = botsNums(0).toInt
          val bot1 = botsNums(1).toInt
          botsStateMap.get(bot1) match {
            case Some(value) =>
              microChips.get(value.botNumber) match {
                case Some(value1) =>
                  microChips.put(value.botNumber, value1 :+ microChip)
                case None =>
                  microChips.put(value.botNumber, Seq(microChip))
              }
            case None =>
              val bot = BotGeneric(bot1, -1, -1)
              botsStateMap.put(bot1, bot)
              microChips.get(bot.botNumber) match {
                case Some(value1) =>
                  microChips.put(bot.botNumber, value1 :+ microChip)
                case None =>
                  microChips.put(bot.botNumber, Seq(microChip))
              }
          }
        case None => ()
      }
    }

    def parseBotVsBot(botsNums: List[String], line: String) = {
      botGivesLowAndHighRegExp.findFirstMatchIn(line) match {
        case Some(value) =>
          val bot1 = botsNums(0).toInt
          val bot2 = botsNums(1).toInt
          val bot3 = botsNums(2).toInt

          botsStateMap.get(bot1) match {
            case Some(value) if value.heGivesLowerToThis < 0 =>
              botsStateMap.put(bot1, BotGeneric(bot1, bot2, bot3))
            case Some(value) if value.heGivesLowerToThis >= 0 =>
              ()
            case None =>
              val bot = BotGeneric(bot1, bot2, bot3)
              botsStateMap.put(bot1, bot)
          }
        case None => ()
      }
    }

  }

}
