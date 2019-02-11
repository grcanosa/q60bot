package com.grcanosa.q60bot

object Q60App extends App {

import com.grcanosa.q60bot.utils.Q60Utils._


  questions.foreach { q =>
    println("QUESTION")
    println(q.toString)
  }
}
