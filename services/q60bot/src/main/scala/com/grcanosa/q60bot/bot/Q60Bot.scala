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
import com.grcanosa.q60bot.bot.Q60Bot.SendToAllUsers
import com.grcanosa.q60bot.model.Q60User
import com.bot4s.telegram.api.declarative._

import scala.concurrent.Future

object Q60Bot {

  case class SendToAllUsers(msg: String)

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

  private def atomic[T](f: => T): T = chatActors.synchronized {
    f
  }

  def replyWithPhoto(photo               : InputFile)
//  ,
//                     caption             : Option[String] = None,
//                     disableNotification : Option[Boolean] = None,
//                     replyToMessageId    : Option[Long] = None,
//                     replyMarkup         : Option[ReplyMarkup] = None)
                    (implicit msg: Message): Future[Message] = {
    request(SendPhoto(msg.chat.id, photo))
  }

  // Usage
//  onCommand('pic) { implicit msg =>
//    replyWithPhoto(InputFile(Paths.get("cat.jpg")), "!!")
//  }

  def getHandler(m: Message): ActorRef = atomic {
    mylog.info(s"Getting handler for ${m.chat.id}")
    chatActors.getOrElseUpdate(m.chat.id, {
      system.actorOf(Props(classOf[PlayerActor], Q60User(m.chat.id, m.chat.firstName, m.chat.lastName)), name = s"player${m.chat.id}")
    })
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
    val senderAdmin = msg.from.exists(_.id == rootId)
    if(!senderAdmin){
      reply(BotTexts.rootCmdOnlyText)
    }
    senderAdmin
  }


  onCommand("/start") { implicit m =>
    mylog.info(s"START CMD ${m.chat.id}")
    addedToUsers { handler =>
      mylog.info("Replying Start CMD")
      reply(BotTexts.startText())
    }
  }

  onCommand("/reglas") { implicit m =>
    addedToUsers { handler =>
      reply(BotTexts.reglasText)
    }
  }

  onCommand("/miguefoto") { implicit m => addedToUsers { handler =>
      replyWithPhoto(InputFile(Paths.get(BotTexts.getPhotoPath())))
    }
  }

  onCommand("/broadcast") { implicit m =>
    addedToUsers { handler =>
      isAdmin { admin =>
        withArgs { args => {
            sendToAllUsers(args.toString)
          }
        }
      }
    }
  }

  onMessage { implicit msg =>
    addedToUsers { handler =>
      handler ! msg
    }
  }

  class BotActor extends Actor {

    override def receive = {

      case SendToAllUsers(msg) => sendToAllUsers(msg)
    }
  }


}
