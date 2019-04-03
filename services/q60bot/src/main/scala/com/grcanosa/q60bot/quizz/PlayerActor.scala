package com.grcanosa.q60bot.quizz

import akka.actor.{Actor, ActorRef}
import com.bot4s.telegram.methods.{SendMessage, SendPhoto}
import com.bot4s.telegram.models.{InputFile, Message}
import com.grcanosa.q60bot.bot.BotTexts
import com.grcanosa.q60bot.bot.Q60Bot.{CountDownKeyboard, UserResult}
import com.grcanosa.q60bot.model.{Q60User, Question}
import com.grcanosa.q60bot.quizz.PlayerActor.{NO_QUESTION, QUESTION, QuestionTimeIsOver, STARTING}
import com.grcanosa.q60bot.quizz.QuizzActor.NewQuestionToUsers

import scala.concurrent.duration._

object PlayerActor {
  case object QuestionTimeIsOver
  val STARTING = 1
  val QUESTION = 2
  val NO_QUESTION = 3
}





class PlayerActor(val user: Q60User, val botActor: ActorRef) extends Actor{


  import com.grcanosa.q60bot.utils.Q60Utils._
  import com.grcanosa.q60bot.bot.BotTexts._
  import com.grcanosa.q60bot.bot.BotData._

  implicit val context2 = context.dispatcher

  var state: Int = STARTING

  var points = 0

  var currQuestion: Option[Question] = None

  var currQuestionAnswered = false
  var currQuestionOK = false

  def handleStartingMessage(m:Message) = {
    botActor ! SendMessage(m.chat.id,BotTexts.startText())
  }

  def handleAnswer(txt: String) = {
    if(currQuestionAnswered){
      botActor ! SendMessage(user.chatId,BotTexts.questionAlreadyAnswered,replyMarkup = Some(removeKeyboard))
    }else{
      botActor ! SendMessage(user.chatId,BotTexts.answerReceived,replyMarkup = Some(removeKeyboard))
      currQuestionAnswered = true
      if(txt == currQuestion.get.solution) {
        currQuestionOK = true
        points += currQuestion.map(_.points).getOrElse(0)
      }else {
        currQuestionOK = false
      }
      mylog.info(s"User ${user.chatId} has $points puntos")
    }
  }

  def handleQuestionMessage(m:Message) = {
    m.text.foreach {
      case txt@("A" | "B" | "C" | "D") => handleAnswer(txt)
      case _ => botActor ! SendMessage(m.chat.id, BotTexts.unkownQuizzAnswer)
    }
  }

  def handleNoQuestionMessage(m:Message) = {
    botActor ! SendMessage(m.chat.id,BotTexts.noQuestionRightNow,replyMarkup = Some(removeKeyboard))
  }

  override def receive()={
    case m:Message => {
      mylog.info(s"Processing message ${m.text.get}, state: ${state}")
      if(state == STARTING){
        handleStartingMessage(m)
      }
      else if(state == QUESTION){
        handleQuestionMessage(m)
      }
      else if(state == NO_QUESTION){
        handleNoQuestionMessage(m)
      }
    }

    case NewQuestionToUsers(q, m) => {
      mylog.info(s"Sending question to user ${user.chatId}: ${user.firstName.getOrElse("")}, ${user.lastName.getOrElse("")}")
      currQuestion = Some(q)
      state = QUESTION
      currQuestionAnswered = false
      currQuestionOK = false
      if(q.photo.isDefined)
        botActor ! SendPhoto(user.chatId,InputFile(q.photo.get))
      botActor ! SendMessage(user.chatId,m,replyMarkup = Some(answersKeyboard))
//      context.system.scheduler.scheduleOnce(questionAnswerDelay){
//        self ! QuestionTimeIsOver
//      }
      context.system.scheduler.scheduleOnce(1 seconds) {
        botActor ! CountDownKeyboard(user.chatId, None, questionAnswerDelay - (1 seconds))
      }
    }

    case QuestionTimeIsOver => {
      state = NO_QUESTION
      if(! currQuestionAnswered){
        botActor ! SendMessage(user.chatId,BotTexts.questionNotAnswered,replyMarkup = Some(removeKeyboard))
      }else{
        if(currQuestionOK){
          botActor ! SendMessage(user.chatId,BotTexts.questionAnsweredOK,replyMarkup = Some(removeKeyboard))
        }else{
          botActor ! SendMessage(user.chatId,BotTexts.questionAnsweredKO,replyMarkup = Some(removeKeyboard))
        }
      }
      botActor ! UserResult(user,points)
    }
  }

}