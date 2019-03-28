package com.grcanosa.q60bot.bot

import java.nio.file.Paths

import akka.actor.{Actor, ActorRef, Props}
import com.bot4s.telegram.api.declarative.{Action, Commands}
import com.bot4s.telegram.api._
import com.bot4s.telegram.clients.AkkaHttpClient
import com.bot4s.telegram.methods.{SendMessage, SendPhoto}
import com.bot4s.telegram.models._
import com.grcanosa.q60bot.quizz.{PlayerActor, QuizzActor, Scoreboard}
import com.grcanosa.q60bot.bot.BotTexts
import com.grcanosa.q60bot.bot.Q60Bot.{LoadBotUsers, SaveBotUsers, SendToAllUsers}
import com.grcanosa.q60bot.model.Q60User
import com.bot4s.telegram.api.declarative._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.Future

object Q60Bot {

  case class SendToAllUsers(msg: String)

  case object SaveBotUsers

  case object LoadBotUsers
}




class Q60Bot(val token: String) extends TelegramBot
//  with ActorBroker
  with AkkaDefaults
  with Commands
  with Polling
  with ChatActions {

  import com.grcanosa.q60bot.utils.Q60Utils._

  override val client: RequestHandler = new AkkaHttpClient(token)

  val botActor = system.actorOf(Props(new BotActor), name = "botActor")

  val chatActors = collection.mutable.Map[Long, ActorRef]()

  botActor ! LoadBotUsers

  private def atomic[T](f: => T): T = chatActors.synchronized {
    f
  }

  def replyWithPhoto(photo               : InputFile)
                    (implicit msg: Message): Future[Message] = {
    request(SendPhoto(msg.chat.id, photo))
  }

  def isAdmin(ok: Action[User])(implicit msg: Message): Unit = {
    msg.from.foreach {
      user =>
        if (user.id == rootId)
          ok(user)
        else
          reply(BotTexts.rootCmdOnlyText)
    }
  }

  def addedToUsers(action: Action[ActorRef])(implicit msg: Message): Unit = {
    action(getHandler(msg))
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


  onCommand("/start") { implicit msg =>
    mylog.info(s"START CMD ${msg.chat.id}")
    addedToUsers { handler =>
      mylog.info("Replying Start CMD")
      reply(BotTexts.startText())
    }
  }

  onCommand("/reglas") { implicit msg =>
    addedToUsers { handler =>
      reply(BotTexts.reglasText)
    }
  }

  onCommand("/miguefoto") { implicit msg => addedToUsers { handler =>
      mylog.info("Replying foto")
      val fotopath = getPhotoPath()
      mylog.info(s"FotoPath $fotopath")
      replyWithPhoto(InputFile(Paths.get(fotopath)))
    }
  }

  onCommand("/broadcast") { implicit msg =>
    addedToUsers { handler =>
      isAdmin { admin =>
        withArgs { args => {
            sendToAllUsers(args.toString)
          }
        }
      }
    }
  }

  onMessage { implicit msg:Message =>
    addedToUsers { handler =>
      mylog.info("Handler message")
      handler ! msg
    }
  }


  def getBotUsers(): Seq[Q60User] = {
    chatActors.map(c => c._2.asInstanceOf[PlayerActor].user).toSeq
  }

  def loadBotUsers(botList: Seq[Q60User]) = {
    botList.foreach(getHandler)
  }

  def getHandler(user:Q60User) = {
   chatActors.getOrElseUpdate(user.chatId, {
      system.actorOf(Props(classOf[PlayerActor], user), name = s"player${user.chatId}")
    })
  }

  def getHandler(m: Message): ActorRef = atomic {
    mylog.info(s"Getting handler for ${m.chat.id}")
    chatActors.getOrElseUpdate(m.chat.id, {
      system.scheduler.scheduleOnce(1 second){
        botActor ! SaveBotUsers
      }
      system.actorOf(Props(classOf[PlayerActor], Q60User(m.chat.id, m.chat.firstName, m.chat.lastName)), name = s"player${m.chat.id}")
    })
  }

  class BotActor extends Actor {

    override def receive = {

      case SendToAllUsers(msg) => sendToAllUsers(msg)

      case SaveBotUsers => saveUsers(getBotUsers())

      case LoadBotUsers => loadBotUsers(loadUsers())
    }
  }


}
