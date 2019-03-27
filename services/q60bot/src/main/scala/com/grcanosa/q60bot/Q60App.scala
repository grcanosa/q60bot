package com.grcanosa.q60bot

import com.grcanosa.q60bot.bot.Q60Bot

object Q60App extends App {

  import com.grcanosa.q60bot.utils.Q60Utils._

  val bot = new Q60Bot(botToken)

  bot.run()
  println("Bot is running, press ENTER to stop")
  scala.io.StdIn.readLine()
  bot.shutdown()
}
