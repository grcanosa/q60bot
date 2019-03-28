package com.grcanosa.q60bot.bot

import akka.actor.{Actor, ActorRef, Props, Terminated}
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.api._
import com.bot4s.telegram.clients.SttpClient
import com.bot4s.telegram.methods.SendMessage
import com.bot4s.telegram.models.{Message, Update}
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}

trait PerChatRequests extends ActorBroker with AkkaDefaults {

  override val broker = Some(system.actorOf(Props(new Broker), "broker"))

  class Broker extends Actor {
    val chatActors = collection.mutable.Map[Long, ActorRef]()

    def receive = {
      case u: Update =>
        u.message.foreach { m =>
          val id = m.chat.id
          val handler = chatActors.getOrElseUpdate(m.chat.id, {
            val worker = system.actorOf(Props(new Worker), s"worker_$id")
            context.watch(worker)
            worker
          })
          handler ! m
        }

      case Terminated(worker) =>
        // This should be faster
        chatActors.find(_._2 == worker).foreach {
          case (k, _) => chatActors.remove(k)
        }

      case _ =>
    }
  }

  // Fo every chat a new worker actor will be spawned.
  // All requests will be routed through this worker actor; allowing to maintain a per-chat state.
  class Worker extends Actor {
    def receive = {
      case m: Message =>
        request(SendMessage(m.source, self.toString))

      case _ =>
    }
  }
}

class PerChatRequestsBot(token: String) extends TelegramBot
  with Polling
  with Commands
  with PerChatRequests {

  LoggerConfig.factory = PrintLoggerFactory()
  // set log level, e.g. to TRACE
  LoggerConfig.level = LogLevel.TRACE

  //implicit val backend = SttpBackends.default
  override val client: RequestHandler = new SttpClient(token)

  // Commands work as usual.
  onCommand("/hello") { implicit msg =>
    reply("Hello World!")
  }
}