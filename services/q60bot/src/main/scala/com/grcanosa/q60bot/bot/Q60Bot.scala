package com.grcanosa.q60bot.bot

import akka.actor.{Actor, ActorRef, Props}
import com.bot4s.telegram.api.declarative.{Action, Commands}
import com.bot4s.telegram.api._
import com.bot4s.telegram.clients.AkkaHttpClient
import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{Message, Update, User}
import com.grcanosa.q60bot.quizz.{PlayerActor, QuizzActor, Scoreboard}
import com.grcanosa.q60bot.bot.BotTexts
import com.grcanosa.q60bot.bot.Q60Bot.SendToAllUsers
import com.grcanosa.q60bot.model.Q60User


object Q60Bot {

  case class SendToAllUsers(msg: String)

}


class Q60Bot(val token: String) extends TelegramBot
  with ActorBroker
  with AkkaDefaults
  with Commands
  with Polling {

  import com.grcanosa.q60bot.utils.Q60Utils._

  override val client: RequestHandler = new AkkaHttpClient(token)
  override val broker = Some(system.actorOf(Props(new Broker), name = "brokerq60"))

  val chatActors = collection.mutable.Map[Long, ActorRef]()

  private def atomic[T](f: => T): T = chatActors.synchronized {
    f
  }


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

  class Broker extends Actor {

    override def receive = {
      case u: Update => {
        u.message.foreach { m =>
          mylog.info(s"Getting handler update for ${m.chat.id}")
          getHandler(m) ! m
        }
      }
      case SendToAllUsers(msg) => sendToAllUsers(msg)
    }
  }


}
