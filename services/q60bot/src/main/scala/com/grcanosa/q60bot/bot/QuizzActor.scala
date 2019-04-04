package com.grcanosa.q60bot.bot

import akka.actor.{Actor, ActorRef}
import com.bot4s.telegram.methods.SendMessage
import com.grcanosa.q60bot.bot.BotWithAdminAndPlayers.{SendToAllHandlers, SendToAllUsers}
import com.grcanosa.q60bot.model.{Q60User, Question}


object QuizzActor {

  case object NewQuestion
  case class NewQuestion(question: Question, msg: String)
  case class UserResult(user:Q60User,result: Int)
  case object GetResults
}


class QuizzActor(val botActor: ActorRef) extends Actor{

  import QuizzActor._
  import com.grcanosa.q60bot.bot.BotData._
  import com.grcanosa.q60bot.utils.Q60Utils._

  val userResults = collection.mutable.Map[Long,UserResult]()

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

  def handleUserResult(ur: UserResult) = {
    userResults.put(ur.user.chatId,ur)
  }

  override def receive = {
    case NewQuestion => {
      mylog.info("Sending new question to users")
      val q = getCurrentQuestion()
      val m = getCurrentQuestionMessage()
      botActor ! SendToAllHandlers(NewQuestion(q,m))
      nextQuestion()
    }

    case ur: UserResult => handleUserResult(ur)

    case GetResults => {
      mylog.info("Getting Results")
      val txtmsg = userResults.toSeq.sortBy(_._2.result).map(_._2).reverse.zipWithIndex.map{
        case (ur, ind) => BotTexts.getResultsText(ind+1,ur)
      }.mkString("\n")
      botActor ! SendToAllUsers(Some(SendMessage(0.toLong,txtmsg)))
    }


  }
}
