package com.grcanosa.q60bot.quizz

import akka.actor.{Actor, ActorRef}
import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{Message, Update}
import com.grcanosa.q60bot.model.{Q60User, Question}
import com.grcanosa.q60bot.quizz.QuizzActor.{SendBroadcast, SendQuestion, SendResults}

object QuizzActor {

  case object SendQuestion
  case object SendResults
  case class SendBroadcast(msg:String)

}

object QuizzActorState extends Enumeration {
  type QuizzActorState = Value
  val STARTING, QUESTION, NO_QUESTION = QuizzActorState

}

class QuizzActor(val botActor: ActorRef) extends Actor{

  import QuizzActorState._
  import Scoreboard._

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
