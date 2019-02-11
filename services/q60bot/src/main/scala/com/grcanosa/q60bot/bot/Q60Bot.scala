package com.grcanosa.q60bot.bot

import akka.actor.{Actor, ActorRef, Props}
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.api._
import com.bot4s.telegram.clients.AkkaHttpClient
import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.Update

class Q60Bot(val token: String, quizzActor: ActorRef) extends TelegramBot
with ActorBroker
with AkkaDefaults
with Commands
with Polling {

import com.grcanosa.q60bot.utils.Q60Utils._

  override val client: RequestHandler = new AkkaHttpClient(token)
  override val broker = Some(system.actorOf(Props(new Broker),name="brokerq60"))

  class Broker extends Actor {


    override def receive = {
      case u: Update => {
        u.message.foreach( m =>
        quizzActor ! m)
      }

      case sm: SendMessage => request(sm)
    }

  }
}
