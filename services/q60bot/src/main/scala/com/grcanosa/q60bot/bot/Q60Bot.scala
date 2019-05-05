package com.grcanosa.q60bot.bot

import akka.actor.Props
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.methods._
import com.bot4s.telegram.models._

import scala.util.{Failure, Success}
import com.grcanosa.q60bot.bot.PlayerActor.{ChangeName, QuizzActorRef, SendHelpText, SendStartText}
import com.grcanosa.q60bot.bot.QuizzActor.{GetResults, NewQuestion, TestQuestion}







class Q60Bot(token: String, rootId: Long, dev: Boolean)
  extends BotWithAdminAndPlayers(token,rootId,dev)
    with Commands{

  import BotWithAdminAndPlayers._
  import com.grcanosa.q60bot.utils.Q60Utils._
  val quizzActor = system.actorOf(Props(new QuizzActor(botActor)), name = "quizzActor")



  botActor ! LoadBotUsers

  onCommand("/help"){ implicit msg =>
    addedToUsers { handler =>
      handler ! SendHelpText
    }
  }

  onCommand("/start") { implicit msg =>
    addedToUsers { handler =>
      handler ! SendStartText
    }
  }

  onCommand("/reglas") { implicit msg =>
    addedToUsers { _ =>
      reply(BotTexts.reglasText,replyMarkup = Some(BotTexts.removeKeyboard))
    }
  }

  onCommand("/miguefoto") { implicit msg =>
    addedToUsers { _ =>
      replyWithPhoto(BotData.getPhotoPath())
    }
  }

  onCommand("/nombre") { implicit msg =>
    addedToUsers { handler =>
      handler ! ChangeName
    }
  }

  //ADMIN COMMANDS
  //Broadcast
  onCommand("/b") { implicit msg =>
    addedToUsers { _ =>
      isAdmin { _ =>
        msg.text.foreach{
          case "/b" => mylog.info("Broadcast comment without append, ignoring")
          case s => botActor ! SendToAllUsers(Some(SendMessage(0.toLong,s.replace("/b",""))),None)
        }
      }
    }
  }

  onCommand("/q") { implicit msg =>
    addedToUsers { _ =>
      isAdmin { _ =>
        //This way we ensure all handlers have the quizzActorRef before a question
        botActor ! SendToAllHandlers(QuizzActorRef(quizzActor))
        quizzActor ! NewQuestion
      }
    }
  }

  onCommand("/tq") { implicit msg =>
    addedToUsers { _ =>
      isAdmin { _ =>
        botActor ! SendToAllHandlers(QuizzActorRef(quizzActor))
        quizzActor ! TestQuestion
      }
    }
  }

  onCommand("/r") { implicit msg =>
    addedToUsers{ _ =>
      isAdmin { _ =>
        quizzActor ! GetResults
      }
    }
  }

  onCommand("/u") { implicit msg =>
    addedToUsers  { _ =>
      isAdmin { _ =>
        val txtMsg = BotTexts.userListText(chatHandlers.map{
                                        case (_,ch) => ch.user.displayName
                                        }.toSeq)
        msg.text.foreach{
          case "/u" => botActor ! SendMessage(rootId,(":ok_hand: "+txtMsg).emojize)
          case "/u ALL" => botActor ! SendToAllUsers(Some(SendMessage(0.toLong,txtMsg)))
        }
      }
    }
  }

  onMessage { implicit msg:Message =>
    addedToUsers { handler =>
      isNotCommand { _ =>
        if(msg.photo.isDefined){
          handlePhotoMessage(msg)
        }else {
          handler ! msg
        }
      }
    }
  }

  def handlePhotoMessage(msg:Message) = {
    mylog.info("photo is defined")
    val photoId = msg.photo.get.reverse.head.fileId
    request(GetFile(photoId)).onComplete{
      case Success(file) =>
        file.filePath match {
          case Some(filePath) =>
            // See https://core.telegram.org/bots/api#getfile
            val url = s"https://api.telegram.org/file/bot$token/$filePath"
            BotData.downloadFile(url,filePath)
            val txt = "Enviado por: "+getChatHandler(msg).user.displayName
            botActor ! SendToAllUsersExcept(msg.chat.id,None,Some(SendPhoto(0.toLong,photo=InputFile(photoId),caption=Some(txt))))
          case _ =>
        }
      case Failure(e) =>
        logger.error("Exception: " + e)
    }
  }

}
