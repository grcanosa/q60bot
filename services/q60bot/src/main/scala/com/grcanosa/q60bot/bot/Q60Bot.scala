package com.grcanosa.q60bot.bot

import akka.actor.Props
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.methods._
import com.bot4s.telegram.models._

import scala.util.{Failure, Success}
import com.grcanosa.q60bot.bot.PlayerActor.{ChangeName, QuizzActorRef}
import com.grcanosa.q60bot.bot.QuizzActor.{GetResults, NewQuestion}







class Q60Bot(token: String, rootId: Long, dev: Boolean)
  extends BotWithAdminAndPlayers(token,rootId,dev)
    with Commands{

  import BotWithAdminAndPlayers._
  import com.grcanosa.q60bot.utils.Q60Utils._
  val quizzActor = system.actorOf(Props(new QuizzActor(botActor)), name = "quizzActor")



  botActor ! LoadBotUsers


  onCommand("/start") { implicit msg =>
    addedToUsers { _ =>
      reply(BotTexts.startText(),replyMarkup = Some(BotTexts.removeKeyboard))
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
        msg.text.foreach(s =>
          botActor ! SendToAllUsers(Some(SendMessage(0.toLong,s.replace("/b ",""))),None)
        )
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
    request(GetFile(msg.photo.get.head.fileId)).onComplete{
      case Success(file) =>
        file.filePath match {
          case Some(filePath) =>
            // See https://core.telegram.org/bots/api#getfile
            val url = s"https://api.telegram.org/file/bot$token/$filePath"
            BotData.downloadFile(url,filePath)
          case _ =>
        }
      case Failure(e) =>
        logger.error("Exception: " + e)
    }
  }

}
