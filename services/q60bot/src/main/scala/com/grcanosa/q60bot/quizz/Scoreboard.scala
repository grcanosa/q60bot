package com.grcanosa.q60bot.quizz

import com.bot4s.telegram.models.ChatId
import com.grcanosa.q60bot.model.Q60User

object Scoreboard {

  var users = scala.collection.immutable.Seq.empty[Q60User]

  def exitsUser(chatId:ChatId) = {
     users.map(u => u.chatId == chatId).exists(b => b)
  }

  def insertUser(chatId:ChatId, firstName: Option[String], lastName: Option[String]) = {
    users = users :+ Q60User(chatId,0,firstName, lastName)
  }

  def insertUserIfNotExists(chatId:ChatId,firstName: Option[String], lastName: Option[String]) = {
    val exists = exitsUser(chatId)
    if(! exists) insertUser(chatId,firstName, lastName)
    ! exists
  }

  def getResultsString(): String = {
    ""
  }
}
