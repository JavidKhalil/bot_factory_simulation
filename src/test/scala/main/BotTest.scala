package main

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import output.BotProcessingFactory.Parsers._
import output.BotProcessingFactory._
import output.Patterns._

class BotTest extends AnyWordSpec with Matchers {

  val valuesLine = "value 59 goes to bot 6"
  val botLine = "bot 135 gives low to bot 29 and high to bot 146"
  val outputLine = "bot 37 gives low to output 20 and high to bot 103"

  "BotProcessing" should {

    "correctly parse instruction file with bot vs bot " in {
      assert(regexpNums.findAllIn(botLine).toList.size == 3)
      assert(botGivesLowAndHighRegExp.findAllIn(botLine).toList.nonEmpty)
    }

    "correctly parse instruction file with bot vs output " in {
      assert(regexpOutputBot.findFirstMatchIn(outputLine).toList.nonEmpty)
    }

    "correctly parse instruction file bot values " in {
      assert(valueGoes.findFirstMatchIn(valuesLine).toList.nonEmpty)
    }

  }
}
