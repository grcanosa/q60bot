package com.grcanosa.q60bot.model

import com.bot4s.telegram.models.ChatId

case class Q60User(chatId: ChatId, points: Int) {

  def addPoints(p:Int) ={
    this.copy(points=points+p)
  }
}