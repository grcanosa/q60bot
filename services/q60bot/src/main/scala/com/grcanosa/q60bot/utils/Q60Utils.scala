package com.grcanosa.q60bot.utils


import com.typesafe.config.{Config, ConfigFactory}
import com.vdurmont.emoji.EmojiParser
import org.slf4j.LoggerFactory


object Q60Utils {

  val mylog  = LoggerFactory.getLogger("q60logger")

  lazy val config: Config = {
    ConfigFactory.load("application.conf")
  }


  implicit class RandomFromList(li: Seq[Any]) {
    import scala.util.Random
    val random = new Random
    def chooseRandom(): Option[Any] = li.length match {
      case 0 => None
      case _ => Some(li(random.nextInt(li.length)))
    }
  }

  implicit class RandomFromStringList(li: Seq[String]) {
    import scala.util.Random
    val random = new Random
    def chooseRandomStr(): String = li.chooseRandom().getOrElse("").toString
  }

  implicit class EmojiString(s: String){
    def emojize: String = EmojiParser.parseToUnicode(s)
  }

}
