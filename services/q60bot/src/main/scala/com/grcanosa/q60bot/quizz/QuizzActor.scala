package com.grcanosa.q60bot.quizz

import akka.actor.{Actor, ActorRef}
import com.grcanosa.q60bot.model.{Q60User, Question}
import com.grcanosa.q60bot.quizz.QuizzActor.{NewQuestion, NewQuestionToUsers}

object QuizzActor {

  case object NewQuestion
  case class NewQuestionToUsers(question: Question, msg: String)

}


class QuizzActor(val botActor: ActorRef) extends Actor{

  import com.grcanosa.q60bot.bot.BotData._
  import com.grcanosa.q60bot.utils.Q60Utils._

  var currQuestionIndex = 1.toInt

  def getCurrentQuestion() = allQuestions(currQuestionIndex-1)


  def getCurrentQuestionMessage():String = {
    val q = getCurrentQuestion()
    val msg =  s"""
        |Pregunta $currQuestionIndex de ${allQuestions.size} (${q.points} puntos):
        |${q.question}
        |A) ${q.respA}
        |B) ${q.respB}
        |C) ${q.respC}
        |D) ${q.respD}
      """.stripMargin
    msg
  }

  def nextQuestion(): Unit ={
    currQuestionIndex += 1
  }

  override def receive = {
    case NewQuestion => {
      mylog.info("Sending new question to users")
      val q = getCurrentQuestion()
      val m = getCurrentQuestionMessage()
      botActor ! NewQuestionToUsers(q,m)
      nextQuestion()
    }


  }
}
