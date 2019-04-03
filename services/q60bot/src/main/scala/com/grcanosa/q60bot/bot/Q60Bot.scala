package com.grcanosa.q60bot.bot

import java.nio.file.Paths

import akka.actor.{Actor, ActorRef, Props}
import com.bot4s.telegram.api.declarative.{Action, Commands}
import com.bot4s.telegram.api._
import com.bot4s.telegram.clients.AkkaHttpClient
import com.bot4s.telegram.methods._
import com.bot4s.telegram.models._
import com.grcanosa.q60bot.quizz.{PlayerActor, QuizzActor}
import com.grcanosa.q60bot.bot.Q60Bot._
import com.grcanosa.q60bot.model.Q60User
import com.bot4s.telegram.api.declarative._
import com.grcanosa.q60bot.bot.BotTexts.removeKeyboard
import com.grcanosa.q60bot.quizz.QuizzActor.{NewQuestion, NewQuestionToUsers}
import com.vdurmont.emoji.EmojiParser
import io.github.todokr.Emojipolation._
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import com.grcanosa.q60bot.quizz.PlayerActor.QuestionTimeIsOver
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Failure, Success}

import sys.process._
import java.net.URL
import java.io.File




object Q60Bot {

  def fileDownloader(url: String, filename: String) = {
    new URL(url) #> new File(filename) !!
  }

  case class SendToAllUsers(msg: String)

  case object SaveBotUsers

  case object LoadBotUsers

  case class CountDownKeyboard(chatId:Long,msgId:Option[Int],duration: FiniteDuration)

  case class UserResult(user:Q60User,result: Int)
  case object GetResults
}




class Q60Bot(val token: String, val dev: Boolean) extends TelegramBot
//  with ActorBroker
  with AkkaDefaults
  with Commands
  with Polling
  with ChatActions {

  import com.grcanosa.q60bot.utils.Q60Utils._
  import com.grcanosa.q60bot.bot.BotData._

  override val client: RequestHandler = new AkkaHttpClient(token)

  val botActor = system.actorOf(Props(new BotActor), name = "botActor")
  val quizzActor = system.actorOf(Props(new QuizzActor(botActor)), name = "quizzActor")
  val chatActors = collection.mutable.Map[Long, (ActorRef,Q60User)]()
  val userResults = collection.mutable.Map[Long,UserResult]()
  val photosIds = collection.mutable.Map[String, String]()

  botActor ! LoadBotUsers

  private def atomic[T](f: => T): T = chatActors.synchronized {
    f
  }

  def replyWithPhoto(photoPath: String)
                    (implicit msg: Message) = {
    val inputFile = photosIds.get(photoPath) match {
      case Some(photoId) => InputFile(photoId)
      case None => InputFile(Paths.get(photoPath))
    }
    val futMsg = request(SendPhoto(msg.chat.id, inputFile,replyMarkup = Some(removeKeyboard)))
      futMsg
          .map(_.photo)
          .collect{case Some(a) if a.nonEmpty => a}
          .map(lp => (lp.head,photoPath))
              .foreach{case (photoSize,path) => photosIds.getOrElseUpdate(path,photoSize.fileId)}

    futMsg.map(printPhotoId)


  }

  def isAdmin(ok: Action[User])(implicit msg: Message): Unit = {
    msg.from.foreach {
      user =>
        if (user.id == rootId)
          ok(user)
        else
          reply(BotTexts.rootCmdOnlyText,replyMarkup = Some(removeKeyboard))
    }
  }

  def isNotCommand(action: Action[Any])(implicit msg:Message) = {
    if(msg.text.exists(!_.startsWith("/")))
      action()
  }

  def addedToUsers(action: Action[ActorRef])(implicit msg: Message): Unit = {
    action(getActorRef(msg))
  }


  def sendToAllUsers(s: String) = {
    chatActors foreach {
      case (chatId, actorRef) => request(SendMessage(chatId, s))
    }
  }

  def isSenderAdmin(msg: Message): Boolean = {
    implicit val msg2 = msg
    val senderAdmin = msg.from.exists(_.id == rootId)
    if(!senderAdmin){
      reply(BotTexts.rootCmdOnlyText)
    }
    senderAdmin
  }

  def isNotCommand(msg:Message): Boolean = {
    msg.text.exists(!_.startsWith("/")) || msg.text.isEmpty
  }


  onCommand("/start") { implicit msg =>
    mylog.info(s"START CMD ${msg.chat.id}")
    addedToUsers { handler =>
      mylog.info("Replying Start CMD")
      reply(BotTexts.startText(),replyMarkup = Some(removeKeyboard))
    }
  }

  onCommand("/reglas") { implicit msg =>
    addedToUsers { handler =>
      reply(BotTexts.reglasText,replyMarkup = Some(removeKeyboard))
    }
  }

  onCommand("/miguefoto") { implicit msg => addedToUsers { handler =>
      mylog.info("Replying foto")
      val fotopath = getPhotoPath()
      mylog.info(s"FotoPath $fotopath")
      replyWithPhoto(fotopath)
    }
  }

  onCommand("/name") { implicit msg => addedToUsers { handler =>
      mylog.info("Changing name")
    }
  }

  onCommand("/b") { implicit msg =>
    addedToUsers { handler =>
      isAdmin { admin =>
        msg.text.foreach(s => sendToAllUsers(s.replace("/b ","")))
      }
    }
  }

  onCommand("/q") { implicit msg =>
    addedToUsers { handler =>
      isAdmin { admin =>
        quizzActor ! NewQuestion
     }
    }
  }

  onCommand("/r") { implicit msg =>
    addedToUsers { handler =>
      isAdmin { admin =>
        val msg = getUsersOrderedByPoints().zipWithIndex.map{
            case (ur,ind) => val a = BotTexts.getResultsText(ind+1,ur)
              mylog.info(s"$ur")
            mylog.info(s"Text: $a")
            a
        }.mkString("\n")
        sendToAllUsers(msg)
      }
    }
  }

  onCommand("/u"){ implicit msg =>
    addedToUsers { handler =>
      isAdmin { admin =>
        val msg2 = chatActors.map { case (chatId,(aref,user)) =>
          s"${user.chatId} - ${user.firstName.getOrElse("")} - ${user.lastName.getOrElse("")} - ${user.username.getOrElse("")}"
        }.mkString("\n")
        reply(msg2)
      }
    }
  }




  onMessage { implicit msg:Message =>
    addedToUsers { handler =>
      //isNotCommand { _ =>
        mylog.info("Handler message")
        if(msg.photo.isDefined){
          mylog.info("photo is defined")
          request(GetFile(msg.photo.get.head.fileId)).onComplete{
            case Success(file) =>
              file.filePath match {

                case Some(filePath) =>
                  // See https://core.telegram.org/bots/api#getfile
                  val url = s"https://api.telegram.org/file/bot${token}/${filePath}"

                  fileDownloader(url,"test.png")

                case None =>
                  println("No file_path was returned")
              }

            case Failure(e) =>
              logger.error("Exception: " + e) // poor's man logging
          }
        }else {
          handler ! msg
        }
      }

    //}
  }


  def getBotUsers(): Seq[Q60User] = {
    chatActors.map(_._2._2).toSeq
  }

  def loadBotUsers(botList: Seq[Q60User]) = {
    botList.foreach(getActorRef)
  }

  def getActorRef(user:Q60User) = atomic {
   chatActors.getOrElseUpdate(user.chatId, {
      val actorref = system.actorOf(Props(classOf[PlayerActor], user, botActor), name = s"player${user.chatId}")
     (actorref,user)
    })._1
  }

def getActorRef(chatId: Long) = {
    chatActors.get(chatId)
}

  def getActorRef(m: Message): ActorRef = atomic {
    mylog.info(s"Getting handler for ${m.chat.id}")
    chatActors.getOrElseUpdate(m.chat.id, {
      system.scheduler.scheduleOnce(1 second){
        botActor ! SaveBotUsers
      }
      val user = Q60User(m.chat.id, m.chat.firstName, m.chat.lastName,m.chat.username)
      val actorRef =system.actorOf(Props(classOf[PlayerActor],
        user,botActor), name = s"player${m.chat.id}")
      (actorRef,user)
    })._1
  }

  def updateResults(ur:UserResult) = {
    userResults.put(ur.user.chatId,ur)
  }

  def getUsersOrderedByPoints() ={
    userResults.toSeq.sortBy(_._2.result).map(_._2).reverse
  }

  def printPhotoId(m:Message) = {
    m.photo.foreach(sp => sp.foreach{
      photoSize => mylog.info(s"Photo ID is ${photoSize.fileId}")
    })
  }

  class BotActor extends Actor {

    override def receive = {

      case sm : SendMessage => request(sm)
      case sp : SendPhoto => request(sp)

      case SendToAllUsers(msg) => sendToAllUsers(msg)

      case SaveBotUsers => saveUsers(getBotUsers(),dev)

      case LoadBotUsers => loadBotUsers(loadUsers(dev))

      case ur: UserResult => updateResults(ur)

      case newQuestion: NewQuestionToUsers => {
        chatActors.foreach{
          case (_,(actorRef,user)) => {
            mylog.info(s"Sending a question to user ${user.chatId}")
            actorRef ! newQuestion
          }
        }
      }

      case CountDownKeyboard(chatId,msgId,duration) => {
        msgId match {
          case None => {
            val msgFuture = request(SendMessage(chatId,"Tiempo restante",replyMarkup = Some(getInlineKeyboard(duration))))
            msgFuture.onComplete{
              case Failure(exception) => mylog.error(s"Problem: ${exception.toString}",exception)
              case Success(msg) => {
                //mylog.info(s"Obtained MESSAGE WITH ID: ${msg.messageId}")
                system.scheduler.scheduleOnce(1 seconds) {
                  self ! CountDownKeyboard(chatId, Some(msg.messageId), duration - (1 seconds))
                }
              }
            }
          }
          case _ => {
            if(duration > (0 seconds)){
              request(EditMessageReplyMarkup(Some(chatId),msgId,replyMarkup = Some(getInlineKeyboard(duration))))
              system.scheduler.scheduleOnce(1 seconds){
                self ! CountDownKeyboard(chatId,msgId, duration - (1 seconds))
              }
            }else{
              request(DeleteMessage(chatId,msgId.get))
              getActorRef(chatId).foreach{ case (ar,u) => ar ! QuestionTimeIsOver}
            }

          }
        }
      }
    }

    def getInlineKeyboard(duration:FiniteDuration) = {
      val keytxt = duration.toSeconds.toInt match {
          case 5 => BotTexts.t5sec
          case 4 => BotTexts.t4sec
          case 3 => BotTexts.t3sec
          case 2 => BotTexts.t2sec
          case 1 => BotTexts.t1sec
          case n => n.toString+" segundos"
        }
      //mylog.info(s"String is $keytxt")
      val keyboard = InlineKeyboardButton(keytxt, callbackData = Some("A"))
      InlineKeyboardMarkup(Seq(Seq(keyboard)))
    }
  }


}
