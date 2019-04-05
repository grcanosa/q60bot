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

  import com.grcanosa.q60bot.utils.Q60Utils._

  val fechacumple = LocalDateTime.of(2019,4,28,13,0,0)

  def startText() = {

    val diff = Duration.between(LocalDateTime.now(),fechacumple)
    val days: Long = diff.getSeconds/(3600*24)
    val hours: Long = diff.minusDays(days).getSeconds/3600

    val s = s"""
        Hola, soy el bot encargado del cumpleaños de Miguel Ángel!! :birthday:
               |El cumpleaños se celebrará el día 28 de Abril a partir de las 13:00 horas, quedan ${diff.getSeconds.toString} segundos (${days.toString} dias y ${hours.toString} horas) para el cumpleaños.
               |Debéis traer ganas de comer :hamburger: :pizza: :shallow_pan_of_food:, beber :cocktail: :wine_glass: :tropical_drink: :beer: y pasarlo bien :tada: :confetti_ball: :balloon:.
""".stripMargin.emojize
  }

  val reglasText =
    """
      |Las reglas de la fiesta son las siguientes:
      |1) No se habla de política o de las elecciones.
      |2) No se habla de política o de las elecciones.
      |3) No se habla de política o de las elecciones.
    """.stripMargin

  def unkownCmdText: String = Seq(
    "No conozco ese comando... :cry:" ,
    "Ahí me has pillado... xD :flushed_face:",
    "No se de qué me hablas...",
    "No te entiendo, lo siento..."
  ).chooseRandomStr().emojize

  val quizzNotStartedYet = "El Q60 no ha empezado todavía...".emojize

  val rootCmdOnlyText = Seq(
    "Comando solo permitido para el administrador.",
    "No has visto que ponía RESTRICTED??"
  ).chooseRandomStr().emojize

  val unkownQuizzAnswer = "La respuesta tiene que ser A,B,C o D :sob:".emojize

  def questionAlreadyAnswered = Seq(
    "Ya has contestado a la pregunta, tamprosill@... :disappointed: ",
    "Uy, uy, uy, uy.... Qué tramposo... Solo puedes contestar una vez..."
  ).chooseRandomStr().emojize

  def questionNotAnswered = Seq(
    "No has respondido la pregunta... Hay que se más rápido la próxima vez...",
    "Pero si no has respondido!! Elige al azar aunque sea, tienes un 25% de posibilidades de acertar!"
  ).chooseRandomStr().emojize

  def questionAnsweredOK = Seq(
    "Enhorabuena!! Pregunta acertada!",
    ":tada::tada::tada::tada::confetti_ball::confetti_ball::confetti_ball::confetti_ball:"
  ).chooseRandomStr().emojize

  def questionAnsweredKO = Seq(
    "Ohhh... La próxima seguro que aciertas...",
    "Vaya :sad:, ánimo que ésta era difícil!"
  ).chooseRandomStr().emojize

  def answerReceived = Seq(
    "Respuesta recibida!",
    "Ok!",
    "Claro.",
    "Me lo apunto"
  ).chooseRandomStr().emojize

  val noQuestionRightNow = emoji"Espera un poco a qué te haga una pregunta... :wink:"

  val cannotChangeNameRightNow = emoji"Ahora mismo no puedes cambiar tu nombre en la clasificación!"

  val requestNewName = "Dime como quieres llamarte en la clasificación."

  def changeNameAccepted = Seq(
    "Perfecto!",
    "Entendido!",
    "Muy bien.",
    "Ok"
  ).chooseRandomStr().emojize

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
