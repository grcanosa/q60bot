package com.grcanosa.q60bot.model

import com.bot4s.telegram.models.ChatId



case class Question(question:String, points: Int,
                    respA: String,
                    respB: String,
                    respC: String,
                    respD: String,
                    solution: String,
                    photo: Option[String] = None)
