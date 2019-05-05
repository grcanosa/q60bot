package com.grcanosa.q60bot.bot

import akka.actor.{Actor, ActorRef}
import com.bot4s.telegram.methods.SendMessage
import com.grcanosa.q60bot.bot.BotWithAdminAndPlayers.{SendToAllHandlers, SendToAllUsers}
import com.grcanosa.q60bot.bot.PlayerActor.UpdateUser
import com.grcanosa.q60bot.model.{Q60User, Question}

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._

object QuizzActor {

  case object NewQuestion
  case object TestQuestion
  case class NewQuestion(question: Question, msg: String)
  case class UserResult(user:Q60User,points: Int,success: Boolean)
  case class SendQuestionResponse(question:Question)
  case object GetResults
  case object AnalyzeQuestionResults
}


class QuizzActor(val botActor: ActorRef) extends Actor{

  import QuizzActor._
  import com.grcanosa.q60bot.bot.BotData._
  import com.grcanosa.q60bot.utils.Q60Utils._

  implicit val ec = context.system.dispatcher

  val userResults = collection.mutable.Map[Long,UserResult]()

  var questionResults = TrieMap[Long,UserResult]()

  var currQuestionIndex = 1.toInt

  def getCurrentQuestion() = allQuestions(currQuestionIndex-1)


  def getCurrentQuestionMessage():String = {
    val q = getCurrentQuestion()
    val msg =  s"""
        |Pregunta $currQuestionIndex (${q.points} puntos):
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

  def emptyQuestionResults() = {
    questionResults = TrieMap[Long,UserResult]()
  }

  def handleUserResult(ur: UserResult) = {
    questionResults.put(ur.user.chatId,ur)
    userResults.put(ur.user.chatId,ur)
  }

  def analyzeQuestionResults() = {
    val badAnswer = questionResults.toSeq.map(_._2).count(_.success == false)
    val goodAnswer = questionResults.toSeq.map(_._2).count(_.success == true)
    botActor ! SendToAllUsers(Some(SendMessage(0.toLong,BotTexts.questionResultsText(goodAnswer,badAnswer))))

  }

  override def receive = {
    case NewQuestion => {
      if(currQuestionIndex <= allQuestions.length) {
        mylog.info("Sending new question to users")
        val q = getCurrentQuestion()
        val m = getCurrentQuestionMessage()
        emptyQuestionResults()
        botActor ! SendToAllHandlers(NewQuestion(q, m))
        context.system.scheduler.scheduleOnce(questionAnswerDelay + (5 seconds)) {
          self ! SendQuestionResponse(q)
          self ! AnalyzeQuestionResults
        }
        nextQuestion()
      }else{
        mylog.info("No more questions")
      }
    }

    case TestQuestion => {
      mylog.info("Sending test question")
      val q = Question("¿De qué color es el caballo blanco de Santiago?",0,"Blanco","Negro","Melón","Rosa","A")
      val msg = s"""
                   |Pregunta de prueba (0 puntos):
                   |${q.question}
                   |A) ${q.respA}
                   |B) ${q.respB}
                   |C) ${q.respC}
                   |D) ${q.respD}
      """.stripMargin
      emptyQuestionResults()
      context.system.scheduler.scheduleOnce(questionAnswerDelay +( 5 seconds)){
        self ! SendQuestionResponse(q)
        self ! AnalyzeQuestionResults
      }
      botActor ! SendToAllHandlers(NewQuestion(q,msg))
    }

    case ur: UserResult => handleUserResult(ur)

    case GetResults => {
      mylog.info("Getting Results")
      val txtmsg = userResults.toSeq.sortBy(_._2.points).map(_._2).reverse.zipWithIndex.map{
        case (ur, ind) => BotTexts.getResultsText(ind+1,ur)
      }.mkString("\n")
      botActor ! SendToAllUsers(Some(SendMessage(0.toLong,txtmsg)))
    }

    case UpdateUser(user) => {
      userResults.get(user.chatId).foreach(ur =>
        userResults.update(user.chatId,ur.copy(user = user))
      )
    }

    case SendQuestionResponse(q) => botActor ! SendToAllUsers(Some(SendMessage(0.toLong,BotTexts.correctAnswerText(q))))

    case AnalyzeQuestionResults => analyzeQuestionResults()


  }
}
