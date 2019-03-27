package com.grcanosa.q60bot.bot

import akka.actor.{Actor, ActorRef, Props}
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.api._
import com.bot4s.telegram.clients.AkkaHttpClient
import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.Update
import com.bot4s.telegram.models.Message
import com.grcanosa.q60bot.quizz.{QuizzActor, Scoreboard}





class Q60Bot(val token: String) extends TelegramBot
with ActorBroker
with AkkaDefaults
with Commands
with Polling {

import com.grcanosa.q60bot.utils.Q60Utils._
  import com.grcanosa.q60bot.quizz.QuizzActor._



  override val client: RequestHandler = new AkkaHttpClient(token)
  override val broker = Some(system.actorOf(Props(new Broker),name="brokerq60"))

  val quizzActor = system.actorOf(Props(new QuizzActor(broker.get)), s"quizzActor")

  class Broker extends Actor {


    def handleCommand(m:Message) = {
      m.text match {
        case Some("/start") => self ! SendMessage(m.chat.id,BotTexts.startText())
        case Some("/reglas") => self ! SendMessage(m.chat.id,BotTexts.reglasText)
        case Some("/question") => handleRootMsg(m)
        case Some("/results") => handleRootMsg(m)
        case Some(txt) if txt startsWith "/broadcast" => handleRootMsg(m)
        case Some(_) => self ! SendMessage(m.chat.id,BotTexts.unkownCmdText)
      }
    }

    def handleRootMsg(m:Message) = {
      if(m.chat.id == rootId){
        m.text match {
          case Some("/question") => quizzActor ! SendQuestion
          case Some("/result") => quizzActor ! SendResults
          case Some(txt) => txt match {
            case txt if txt startsWith "/broadcast" => quizzActor ! SendBroadcast(txt.replaceAllLiterally("/broadcast",""))
            case _ => mylog.info(s"Unkown root cmd: $txt")
          }
          case _ => mylog.info("Unknown root cmd")
        }
      }
      else{
        self ! SendMessage(m.chat.id,BotTexts.rootCmdOnlyText)
      }
    }

    override def receive = {
      case u: Update => {
        u.message.foreach( m => {
          Scoreboard.insertUserIfNotExists(m.chat.id,m.chat.firstName,m.chat.lastName)
          mylog.info(s"Message from chatid: ${m.chat.id}")
            if(m.text.exists(_.startsWith("/"))){
              handleCommand(m)
            } else {
              quizzActor ! m
            }

          }
        )
      }

      case sm: SendMessage => request(sm)
    }

  }
}
