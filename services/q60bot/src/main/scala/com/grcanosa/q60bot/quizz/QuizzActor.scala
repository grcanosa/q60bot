package com.grcanosa.q60bot.quizz

import akka.actor.Actor
import com.bot4s.telegram.models.{Message, Update}
import com.grcanosa.q60bot.model.{Question, Q60User}

object QuizzActor {


}

object QuizzActorState extends Enumeration {
  type QuizzActorState = Value
  val STARTING, QUESTION, NO_QUESTION = QuizzActorState

}

class QuizzActor extends Actor{

  import QuizzActorState._
  import Scoreboard

  var state = STARTING


  def handleStartingMessage(m:Message) = {
    if(Scoreboard.insertUserIfNotExists(m.chat.id)){
      
    }else{

    }
  }

  def handleQuestionMessage(m:Message) = {

  }

  def handleNoQuestionMessage(m:Message) = {

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
  }
}
