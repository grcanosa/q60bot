package com.grcanosa.q60bot.bot

import akka.actor.{Actor, ActorRef, Props}
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.api._
import com.bot4s.telegram.clients.AkkaHttpClient
import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.Update
import com.bot4s.telegram.models.Message





class Q60Bot(val token: String, val rootId: Long, quizzActor: ActorRef) extends TelegramBot
with ActorBroker
with AkkaDefaults
with Commands
with Polling {

import com.grcanosa.q60bot.utils.Q60Utils._
  import com.grcanosa.q60bot.quizz.QuizzActor._


  override val client: RequestHandler = new AkkaHttpClient(token)
  override val broker = Some(system.actorOf(Props(new Broker),name="brokerq60"))

  class Broker extends Actor {


    def handleCommand(m:Message) = {
      m.text match {
        case Some("/start") => self ! SendMessage(m.chat.id,BotTexts.startText())
        case Some("/reglas") => self ! SendMessage(m.chat.id,BotTexts.reglasText)
        case Some(_) => self ! SendMessage(m.chat.id,BotTexts.unkownCmdText)
      }
    }

    def handleRootMsg(m:Message) = {
      m.text match {
        case Some("/question") => quizzActor ! SendQuestion
        case Some("/result") => quizzActor ! SendResults
        case Some(txt) => txt match {
          case txt if txt startsWith "/broadcast" => quizzActor ! SendBroadcast(txt.replaceAllLiterally("/broadcast",""))
        }

      }
    }

    override def receive = {
      case u: Update => {
        u.message.foreach( m =>
          if(m.chat.id == rootId) {
            handleRootMsg(m)
          }
          else{
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
