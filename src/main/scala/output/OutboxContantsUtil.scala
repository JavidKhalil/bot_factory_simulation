package output

import BotProcessingFactory.Statement

  object OutboxContantsUtil {

    // chips we should find the bot, charged for balance them
    val firstChip = 17
    val secondChip = 61
    val seqRes = Seq(0, 1, 2)

    def outputBoxs(seqRes: Seq[Int]) = for {
      outputBOS <- Statement.botOutputStateMap.values.map(v => v.heGivesLowerToThis)
      if seqRes.contains(outputBOS)
      outputOBS <- Statement.outputBotStateMap.values.map(v => v.botNumber)
      if seqRes.contains(outputOBS)
      outputOOSLow <- Statement.outputOutputStateMap.values.map(v => v.heGivesLowerToThis)
      if seqRes.contains(outputOOSLow)
      outputOOSHigh <- Statement.outputOutputStateMap.values.map(v => v.heGivesHighToThis)
      if seqRes.contains(outputOOSHigh)
    } yield outputBOS :: outputOBS :: outputOOSLow :: outputOOSLow :: Nil

  }