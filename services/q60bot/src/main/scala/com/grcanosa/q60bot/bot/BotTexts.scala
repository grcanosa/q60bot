package com.grcanosa.q60bot.bot

import java.io.File
import java.time.LocalDateTime
import java.time.Duration

import com.bot4s.telegram.models._
import com.grcanosa.q60bot.bot.QuizzActor.UserResult
import com.vdurmont.emoji.EmojiParser

import scala.concurrent.duration.FiniteDuration



object BotTexts {
  import io.github.todokr.Emojipolation._


  val fechacumple = LocalDateTime.of(2019,4,28,13,0,0)

  def startText() = {

    val diff = Duration.between(LocalDateTime.now(),fechacumple)
    val days: Long = diff.getSeconds/(3600*24)
    val hours: Long = diff.minusDays(days).getSeconds/3600

    val s = s"""
        Hola, soy el bot encargado del cumpleaños de Miguel Ángel!! :birthday:
               |El cumpleaños se celebrará el día 28 de Abril a partir de las 13:00 horas, quedan ${diff.getSeconds.toString} segundos (${days.toString} dias y ${hours.toString} horas) para el cumpleaños.
               |Debéis traer ganas de comer :hamburger: :pizza: :shallow_pan_of_food:, beber :cocktail: :wine_glass: :tropical_drink: :beer: y pasarlo bien :tada: :confetti_ball: :balloon:.
""".stripMargin
    EmojiParser.parseToUnicode(s)
  }

  val reglasText =
    """
      |Las reglas de la fiesta son las siguientes:
      |1) No se habla de política o las elecciones.
      |2) No se habla de política o las elecciones.
      |3) No se habla de política o las elecciones.
    """.stripMargin

  val unkownCmdText = emoji"No conozco ese comando... :cry:"

  val quizzNotStartedYet = emoji"El Q60 no ha empezado todavía..."

  val rootCmdOnlyText = "Comando solo permitido para el administrador..."

  val unkownQuizzAnswer = emoji"La respuesta tiene que ser A,B,C o D :sob:"

  val questionAlreadyAnswered = emoji"Ya has contestado a la pregunta, tamprosill@... :disappointed: "

  val questionNotAnswered = emoji"No has respondido la pregunta... Hay que se más rápido la próxima vez..."

  val questionAnsweredOK = emoji"Enhorabuena!! Pregunta acertada"

  val questionAnsweredKO = emoji"Ohhh... La próxima seguro que aciertas..."

  val answerReceived = emoji"Respuesta recibida!"

  val noQuestionRightNow = emoji"Espera un poco a qué te haga una pregunta... :wink:"

  val cannotChangeNameRightNow = emoji"Ahora mismo no puedes cambiar tu nombre en la clasificación!"

  val requestNewName = "Dime como quieres llamarte en la clasificación."

  val changeNameAccepted = "Perfecto!"

  val answersKeyboard = ReplyKeyboardMarkup(Seq(
    Seq(KeyboardButton("A"),KeyboardButton("B"))
    ,Seq(KeyboardButton("C"),KeyboardButton("D")))
    ,oneTimeKeyboard = Some(true))

  val removeKeyboard = ReplyKeyboardRemove(true)

  val remainingTime = "Tiempo restante"

  def getPositionText(position:Int)= {

    position match {
      case 1 => EmojiParser.parseToUnicode(":first_place_medal:")
      case 2 => EmojiParser.parseToUnicode(":second_place_medal:")
      case 3 => EmojiParser.parseToUnicode(":third_place_medal:")
      case n if n < 10 => position.toString+" "
      case _ => position.toString
    }
  }

  def getResultsText(position: Int, userR: UserResult) = {
    s"${getPositionText(position)}- ${userR.result.toString} puntos - ${userR.user.displayName}"
  }

  val t5sec = EmojiParser.parseToUnicode(":bomb::bomb::bomb::bomb::bomb:")
  val t4sec = EmojiParser.parseToUnicode(":collision::bomb::bomb::bomb::bomb:")
  val t3sec = EmojiParser.parseToUnicode(":collision::collision::bomb::bomb::bomb:")
  val t2sec = EmojiParser.parseToUnicode(":collision::collision::collision::bomb::bomb:")
  val t1sec = EmojiParser.parseToUnicode(":collision::collision::collision::collision::bomb:")

  def getInlineKeyboard(duration:FiniteDuration) = {
    val keytxt = duration.toSeconds.toInt match {
      case 5 => t5sec
      case 4 => t4sec
      case 3 => t3sec
      case 2 => t2sec
      case 1 => t1sec
      case n => n.toString+" segundos"
    }
    val keyboard = InlineKeyboardButton(keytxt, callbackData = Some("A"))
    InlineKeyboardMarkup(Seq(Seq(keyboard)))
  }

}
