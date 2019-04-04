package com.grcanosa.q60bot

import com.grcanosa.q60bot.bot.Q60Bot

object Q60AppDev extends App {

  import com.grcanosa.q60bot.bot.BotData._

  val bot = new Q60Bot(devToken, rootId,true)

  bot.run()
  println("Bot is running, press ENTER to stop")
  scala.io.StdIn.readLine()
  bot.shutdown()

}
