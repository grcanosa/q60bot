package com.grcanosa.q60bot.model

import com.bot4s.telegram.models.ChatId

case class User(chatId: ChatId, points: Int)

case class Opcion(texto: String, correct: Boolean)

case class Question(question:String, opciones: Seq[Opcion], points: Int)
