package com.grcanosa.q60bot.bot

import akka.actor.{Actor, ActorRef}
import com.bot4s.telegram.methods.{DeleteMessage, EditMessageReplyMarkup, SendMessage, SendPhoto}
import com.bot4s.telegram.models.{InputFile, Message}
import com.grcanosa.q60bot.model.{Q60User, Question}
import akka.pattern.ask
import akka.util.Timeout
import com.grcanosa.q60bot.bot.BotWithAdminAndPlayers.SendMsgAndRet
import com.grcanosa.q60bot.bot.QuizzActor.{NewQuestion, UserResult}

import scala.concurrent.duration._

object PlayerActor {
  case object QuestionTimeIsOver
  case object ChangeName
  val STARTING = 1
  val QUESTION = 2
  val NO_QUESTION = 3
  val CHANGE_NAME = 4

  case class CountdownTimer(msgId:Option[Int],duration: FiniteDuration)
  case class MessageId(msgId:Int)
  case class QuizzActorRef(actorRef: ActorRef)
  case class UpdateUser(user:Q60User)
}





class PlayerActor(iniUser: Q60User, val botActor: ActorRef) extends Actor{

  import com.grcanosa.q60bot.utils.Q60Utils._
  import PlayerActor._

  implicit val context2 = context.dispatcher

  var state: Int = STARTING
  var prev_state: Int = STARTING

  var points = 0

  var quizzActor: Option[ActorRef] = None
  var user = iniUser

  var currQuestion: Option[Question] = None

  var currQuestionAnswered = false
  var currQuestionOK = false


  def changeState(new_state:Int) = {
    prev_state = state
    state = new_state
  }

  def goToLastState() = {
    state = prev_state
  }

  def handleStartingMessage(m:Message) = {
    botActor ! SendMessage(m.chat.id,BotTexts.startText())
  }

  def handleAnswer(txt: String) = {
    if(currQuestionAnswered){
      botActor ! SendMessage(user.chatId,BotTexts.questionAlreadyAnswered,replyMarkup = Some(BotTexts.removeKeyboard))
    }else{
      botActor ! SendMessage(user.chatId,BotTexts.answerReceived,replyMarkup = Some(BotTexts.removeKeyboard))
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
    botActor ! SendMessage(m.chat.id,BotTexts.noQuestionRightNow,replyMarkup = Some(BotTexts.removeKeyboard))
  }

  def handleChangeNameMessage(m:Message) = {
    m.text.foreach(txt =>{
      changeName(txt)
      goToLastState()
      botActor ! SendMessage(user.chatId,BotTexts.changeNameAccepted)
      botActor ! UpdateUser(user)
      quizzActor.foreach(_ ! UpdateUser(user))
    })
  }

  def changeName(name:String) = {
    user = user.copy(firstName=Some(name),lastName = None, username = None)
  }


  def sendNewCountdownTimer(delay: FiniteDuration) = {
    val sm = SendMessage(user.chatId,BotTexts.remainingTime,replyMarkup = Some(BotTexts.getInlineKeyboard(delay)))
    implicit val timeout = Timeout(1 second)
    val fut = botActor ? SendMsgAndRet(sm)
    fut.onComplete(_.getOrElse(MessageId(0)) match {
      case MessageId(0) => context.system.scheduler.scheduleOnce(delay){
        self ! QuestionTimeIsOver
      }
      case MessageId(msgId) => context.system.scheduler.scheduleOnce(1 seconds) {
        self ! CountdownTimer( Some(msgId), delay - (1 seconds))
      }
    })
  }

  def editCountDownTimer(msgId:Int,delay:FiniteDuration) = {
    if(delay > (0 seconds)){
      botActor ! EditMessageReplyMarkup(Some(user.chatId),Some(msgId),replyMarkup = Some(BotTexts.getInlineKeyboard(delay)))
      context.system.scheduler.scheduleOnce(1 seconds){
        self ! CountdownTimer(Some(msgId), delay - (1 seconds))
      }
    }else{
      botActor ! DeleteMessage(user.chatId,msgId)
      self ! QuestionTimeIsOver
    }
  }

  def processNewQuestion(q:Question, m:String) = {
    mylog.info(s"Sending question to user ${user.chatId}: ${user.firstName.getOrElse("")}, ${user.lastName.getOrElse("")}")
    currQuestion = Some(q)
    changeState(QUESTION)
    currQuestionAnswered = false
    currQuestionOK = false
    if(q.photo.isDefined)
      botActor ! SendPhoto(user.chatId,InputFile(q.photo.get))
    botActor ! SendMessage(user.chatId,m,replyMarkup = Some(BotTexts.answersKeyboard))
    context.system.scheduler.scheduleOnce(1 seconds) {
      self ! CountdownTimer(None, BotData.questionAnswerDelay - (1 seconds))
    }
  }

  def questionTimeIsOver() = {
    changeState(NO_QUESTION)
    if(! currQuestionAnswered){
      botActor ! SendMessage(user.chatId,BotTexts.questionNotAnswered,replyMarkup = Some(BotTexts.removeKeyboard))
    }else{
      if(currQuestionOK){
        botActor ! SendMessage(user.chatId,BotTexts.questionAnsweredOK,replyMarkup = Some(BotTexts.removeKeyboard))
      }else{
        botActor ! SendMessage(user.chatId,BotTexts.questionAnsweredKO,replyMarkup = Some(BotTexts.removeKeyboard))
      }
    }
    quizzActor.foreach(_ ! UserResult(user,points))
  }

  override def receive()={
    case m:Message => {
      mylog.info(s"${user.chatId}- Message ${m.text.get}, state: ${state}")
      if(state == STARTING){
        handleStartingMessage(m)
      }
      else if(state == QUESTION){
        handleQuestionMessage(m)
      }
      else if(state == NO_QUESTION){
        handleNoQuestionMessage(m)
      }
      else if(state == CHANGE_NAME){
        handleChangeNameMessage(m)
      }
    }

    case ChangeName => {
      if(state == STARTING || state == NO_QUESTION){
        changeState(CHANGE_NAME)
        botActor ! SendMessage(user.chatId,BotTexts.requestNewName)
      }else{
        botActor ! SendMessage(user.chatId,BotTexts.cannotChangeNameRightNow)
      }
    }

    case NewQuestion(q,m) => processNewQuestion(q,m)

    case QuestionTimeIsOver => questionTimeIsOver()

    case CountdownTimer(msgId,delay) => {
      msgId match {
        case None => sendNewCountdownTimer(delay)
        case Some(id) => editCountDownTimer(id,delay)
      }
    }

    case QuizzActorRef(actorRef) => quizzActor = Some(actorRef)
  }

}