package com.grcanosa.q60bot.quizz

import akka.actor.{Actor, ActorRef}
import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{Message, Update}
import com.grcanosa.q60bot.bot.BotTexts
import com.grcanosa.q60bot.model.{Q60User, Question}
import com.grcanosa.q60bot.quizz.QuizzActor.{SendBroadcast, SendQuestion, SendResults}

object QuizzActor {

  case object SendQuestion
  case object SendResults
  case class SendBroadcast(msg:String)

}


class QuizzActor(val botActor: ActorRef) extends Actor{

  import QuizzActorState._
  import Scoreboard._
  import com.grcanosa.q60bot.utils.Q60Utils._

  var state = STARTING

  var currQuestionIndex = 1.toInt

  def getCurrentQuestion = allQuestions(currQuestionIndex-1)


  def getCurrentQuestionMessage() = {
    val q = getCurrentQuestion
    val m =
      s"""
        |Pregunta $currQuestionIndex de ${allQuestions.size}:
        |${q.question}
        |A) ${q.opciones(0)}
        |B) ${q.opciones(1)}
        |C) ${q.opciones(2)}
        |D) ${q.opciones(3)}
      """.stripMargin
  }


  def handleStartingMessage(m:Message) = {
    botActor ! SendMessage(m.chat.id,BotTexts.quizzNotStartedYet)
  }

  def handleQuestionMessage(m:Message) = {

  }

  def handleNoQuestionMessage(m:Message) = {

  }

  def sendMsgToAllUsers(msg:String) = {
    Scoreboard.users foreach {
      u => botActor ! SendMessage(u.chatId,msg)
    }
  }


  override def receive = {
    case m: Message => {
      if(state == STARTING){
        handleStartingMessage(m)
      }
      else if(state == QUESTION){
        handleQuestionMessage(m)
      }
      else if(state == NO_QUESTION){

      }
    }
    case SendQuestion =>
    case SendResults => sendMsgToAllUsers(Scoreboard.getResultsString())
    case SendBroadcast(msg) => sendMsgToAllUsers(msg)
  }
}
