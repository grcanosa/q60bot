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

  def getHandler(m: Message): ActorRef = {
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

  def addedToUsers(action: Action[Any])(implicit msg: Message): ActorRef = {
    getHandler(msg)
  }


  def sendToAllUsers(s: String) = {
    chatActors foreach {
      case (chatId, actorRef) => request(SendMessage(chatId, s))
    }
  }


  onCommand("/start") { implicit m =>
    addedToUsers { handler =>
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


  //  onCommand("/question") { implicit m =>
  //    addedToUsers { handler =>
  //      isAdmin {
  //        admin => {
  //
  //        }
  //      }
  //    }
  //  }
  //
  //  onCommand("/results") { implicit m =>
  //    addedToUsers { handler =>
  //      isAdmin { admin =>
  //      }
  //    }
  //  }


  class Broker extends Actor {

    override def receive = {
      case u: Update => {
        u.message.foreach { m =>
          getHandler(m) ! m
        }
      }
      case SendToAllUsers(msg) => sendToAllUsers(msg)
    }
  }



  //
  //  import com.grcanosa.q60bot.utils.Q60Utils._
  //  import com.grcanosa.q60bot.quizz.QuizzActor._
  //
  //  override val client: RequestHandler = new AkkaHttpClient(token)
  //  override val broker = Some(system.actorOf(Props(new Broker),name="brokerq60"))
  //
  //  val quizzActor = system.actorOf(Props(new QuizzActor(broker.get)), s"quizzActor")
  //
  //  val chatActors = collection.mutable.Map[Long, ActorRef]()
  //
  //  def getChatHandler(chatId:Long) = {
  //    val handler = chatActors.getOrElseUpdate(chatId, {
  //      val worker = system.actorOf(Props(new PlayerActor), s"worker_$chatId")
  //      broker.context.watch(worker)
  //      worker
  //    })
  //  }
  //
  //  class Broker extends Actor {
  //
  //
  //    def handleCommand(m:Message) = {
  //      m.text match {
  //        case Some("/start") => self ! SendMessage(m.chat.id,BotTexts.startText())
  //        case Some("/reglas") => self ! SendMessage(m.chat.id,BotTexts.reglasText)
  //        case Some("/question") => handleRootMsg(m)
  //        case Some("/results") => handleRootMsg(m)
  //        case Some(txt) if txt startsWith "/broadcast" => handleRootMsg(m)
  //        case Some(_) => self ! SendMessage(m.chat.id,BotTexts.unkownCmdText)
  //      }
  //    }
  //
  //    def handleRootMsg(m:Message) = {
  //      if(m.chat.id == rootId){
  //        m.text match {
  //          case Some("/question") => quizzActor ! SendQuestion
  //          case Some("/result") => quizzActor ! SendResults
  //          case Some(txt) => txt match {
  //            case txt if txt startsWith "/broadcast" => quizzActor ! SendBroadcast(txt.replaceAllLiterally("/broadcast",""))
  //            case _ => mylog.info(s"Unkown root cmd: $txt")
  //          }
  //          case _ => mylog.info("Unknown root cmd")
  //        }
  //      }
  //      else{
  //        self ! SendMessage(m.chat.id,BotTexts.rootCmdOnlyText)
  //      }
  //    }
  //
  //    override def receive = {
  //      case u: Update => {
  //        u.message.foreach( m => {
  //          Scoreboard.insertUserIfNotExists(m.chat.id,m.chat.firstName,m.chat.lastName)
  //          mylog.info(s"Message from chatid: ${m.chat.id}")
  //            if(m.text.exists(_.startsWith("/"))){
  //              handleCommand(m)
  //            } else {
  //              quizzActor ! m
  //            }
  //
  //          }
  //        )
  //      }
  //
  //      case sm: SendMessage => request(sm)
  //    }
  //
  //  }
}
