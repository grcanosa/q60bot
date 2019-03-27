package com.grcanosa.q60bot.model

import com.bot4s.telegram.models.ChatId

case class Q60User(chatId: ChatId, firstName: Option[String], lastName: Option[String])
