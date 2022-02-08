package output

/**
  * patterns we use for extracting required information from instructions
  *
  */
object Patterns {
  val botGivesLowAndHighRegExp = "bot [0-9]+ gives low to bot [0-9]+ and high to bot [0-9]+".r
  val regexpNums = "[0-9]+".r
  val regexpOutputOutput = "bot [0-9]+ gives low to output [0-9]+ and high to output [0-9]+".r
  val regexpBotOutput = "bot [0-9]+ gives low to bot [0-9]+ and high to output [0-9]+".r
  val regexpOutputBot = "bot [0-9]+ gives low to output [0-9]+ and high to bot [0-9]+".r
  val valueGoes = "value [0-9]+ goes to bot [0-9]+".r
}
