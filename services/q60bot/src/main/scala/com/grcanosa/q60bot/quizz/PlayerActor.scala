package com.grcanosa.q60bot.quizz

import akka.actor.Actor
import com.bot4s.telegram.models.Message
import com.grcanosa.q60bot.model.{Q60User, Question}
import com.grcanosa.q60bot.quizz.PlayerActor.{NewQuestion, TimesUp}


object PlayerActor{

  case class NewQuestion(question:Question)
  case object TimesUp
}
object PlayerActorState extends Enumeration  {
  type PlayerActorState = Value
  val STARTING, QUESTION, NO_QUESTION = PlayerActorState
}

class PlayerActor(user: Q60User) extends Actor{

  import PlayerActorState._

  var state = STARTING

  var points = 0.toInt

  var currQuestion: Option[Question] = None

  def handleStartingMessage(m:Message) = {
    botActor ! SendMessage(m.chat.id,BotTexts.quizzNotStartedYet)
  }

  def handleQuestionMessage(m:Message) = {

  }

  def handleNoQuestionMessage(m:Message) = {

  }

  override def receive()={
    case m:Message => {
      if(state == STARTING){
        handleStartingMessage(m)
      }
      else if(state == QUESTION){
        handleQuestionMessage(m)
      }
      else if(state == NO_QUESTION){

      }
    }

    case NewQuestion(question) => {
      currQuestion = Some(question)
    }

    case TimesUp =>
  }

}