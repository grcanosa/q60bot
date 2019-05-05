package com.grcanosa.q60bot.bot

import java.nio.file.Paths

import akka.actor.{Actor, ActorRef, Props}
import com.bot4s.telegram.api._
import com.bot4s.telegram.api.declarative.Action
import com.bot4s.telegram.clients.AkkaHttpClient
import com.bot4s.telegram.methods.{DeleteMessage, EditMessageReplyMarkup, SendMessage, SendPhoto}
import com.bot4s.telegram.models.{InputFile, Message, User}
import com.grcanosa.q60bot.bot.BotTexts._
import com.grcanosa.q60bot.model.Q60User
import com.bot4s.telegram.api.declarative._
import com.grcanosa.q60bot.bot.PlayerActor.{MessageId, UpdateUser}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object BotWithAdminAndPlayers{
  case class SendToAllUsers(sm: Option[SendMessage]= None, sp: Option[SendPhoto] = None)
  case class SendToAllUsersExcept(chatId: Long, sm: Option[SendMessage]= None, sp: Option[SendPhoto] = None)
  case class SendToAllHandlers(msg: Any)
  case class SendToAdmin(msg:String)
  case class ChatHandler(actorRef: ActorRef, user: Q60User)

  case class SendMsgAndRet(sm:SendMessage)

  case object SaveBotUsers
  case object LoadBotUsers
}


class BotWithAdminAndPlayers(val token: String, val rootId: Long, val dev:Boolean) extends TelegramBot
with AkkaDefaults
with Polling
with ChatActions
with Messages{

  import BotWithAdminAndPlayers._

  override val client: RequestHandler = new AkkaHttpClient(token)

  val botActor = system.actorOf(Props(new BotActor), name = "botActor")
  val chatHandlers = collection.mutable.Map[Long, ChatHandler]()
  val photosIds = collection.mutable.Map[String, String]()


  def replyWithPhoto(photoPath: String)(implicit msg: Message) = {
    val inputFile = photosIds.get(photoPath) match {
      case Some(photoId) => InputFile(photoId)
      case None => InputFile(Paths.get(photoPath))
    }
    request(SendPhoto(msg.chat.id, inputFile,replyMarkup = Some(removeKeyboard)))
      .map(_.photo)  //Get Photo Option
      .collect{case Some(a) if a.nonEmpty => a} //ONly Get photo seqs that are not empty
      .map(lp => (lp.head,photoPath)) //Add the path so that when the future resolves you have it
      .foreach{case (photoSize,path) => photosIds.getOrElseUpdate(path,photoSize.fileId)} //Update

  }

  private def atomic[T](f: => T): T = chatHandlers.synchronized {
    f
  }

  def isAdmin(ok: Action[User])(implicit msg: Message): Unit = {
    msg.from.foreach {
      user =>
        if (user.id == rootId)
          ok(user)
        else
          reply(rootCmdOnlyText,replyMarkup = Some(removeKeyboard))
    }
  }

  def addedToUsers(action: Action[ActorRef])(implicit msg: Message): Unit = {
    action(getActorRef(msg))
  }

  def isNotCommand(action: Action[Any])(implicit msg:Message) = {
    if(msg.text.exists(!_.startsWith("/")) || msg.text.isEmpty)
      action()
  }


  def getActorRef(user:Q60User) = atomic {
    chatHandlers.getOrElseUpdate(user.chatId, {
      val actorRef = system.actorOf(Props(classOf[PlayerActor], user, botActor), name = s"player${user.chatId}")
      ChatHandler(actorRef,user)
    }).actorRef
  }

  def getActorRef(chatId: Long) = {
    chatHandlers.get(chatId).map(_.actorRef)
  }

  def getActorRef(m: Message): ActorRef = atomic {
    getChatHandler(m).actorRef
  }

  def getChatHandler(m: Message) = atomic {
    chatHandlers.getOrElseUpdate(m.chat.id, {
      system.scheduler.scheduleOnce(1 second){
        botActor ! SaveBotUsers
      }
      botActor ! SendToAdmin(s"New USER: ${m.chat.firstName.getOrElse("")}, ${m.chat.lastName.getOrElse("")},${m.chat.username.getOrElse("")}")
      val user = Q60User(m.chat.id, m.chat.firstName, m.chat.lastName,m.chat.username)
      val actorRef =system.actorOf(Props(classOf[PlayerActor],
        user,botActor), name = s"player${m.chat.id}")
      ChatHandler(actorRef,user)
    })
  }

  def saveBotUsers() = {
    BotData.saveUsers(chatHandlers.map(_._2.user).toSeq)
  }

  def loadBotUsers() = {
    BotData.loadUsers(dev).foreach(getActorRef)
  }

  class BotActor extends Actor{

    override def receive = {
      case sm: SendMessage => request(sm)
      case sp: SendPhoto => request(sp)
      case em: EditMessageReplyMarkup => request(em)
      case dm: DeleteMessage => request(dm)
      case SendToAdmin(msgstr) => self ! SendMessage(rootId,msgstr)
      case SendToAllUsers(sm,sp) => chatHandlers.foreach{
        case (chatId,_) =>
          sm.foreach(self ! _.copy(chatId=chatId))
          sp.foreach(self ! _.copy(chatId=chatId))
      }
      case SendToAllUsersExcept(cid,sm,sp) => chatHandlers.foreach{
        case (chatId,_) if chatId != cid => {
          sm.foreach(self ! _.copy(chatId = chatId))
          sp.foreach(self ! _.copy(chatId = chatId))
        }
        case _ => println("no resending")
      }
      case SendToAllHandlers(m) => chatHandlers.foreach{
        case (_,chatH) => chatH.actorRef ! m
      }
      case SaveBotUsers => saveBotUsers()
      case LoadBotUsers => loadBotUsers()

      case SendMsgAndRet(sm) => {
        val futMsg = request(sm)
        Await.ready(futMsg,1 second)
        futMsg.value.get match {
          case Failure(_) => sender ! MessageId(0)
          case Success(msg) => sender ! MessageId(msg.messageId)
        }
      }

      case UpdateUser(user) => {
        chatHandlers.get(user.chatId).foreach(ch =>
          chatHandlers.update(user.chatId,ch.copy(user = user))
        )
        self ! SaveBotUsers
      }

    }
  }
}
