package com.grcanosa.q60bot.bot

import java.io.File
import java.time.LocalDateTime
import java.time.Duration

import com.bot4s.telegram.models._
import com.grcanosa.q60bot.bot.QuizzActor.UserResult
import com.grcanosa.q60bot.model.Question
import com.vdurmont.emoji.EmojiParser

import scala.concurrent.duration.FiniteDuration



object BotTexts {
  import io.github.todokr.Emojipolation._

  import com.grcanosa.q60bot.utils.Q60Utils._

  val fechacumple = LocalDateTime.of(2019,4,28,13,0,0)

  def startText()(implicit name: String) = {

    val diff = Duration.between(LocalDateTime.now(),fechacumple)
    val days: Long = diff.getSeconds/(3600*24)
    val hours: Long = diff.minusDays(days).getSeconds/3600

    s"""
        Hola $name, soy el bot encargado del cumpleaños de Miguel Ángel!! :birthday:
               |El cumpleaños se celebrará el día 28 de Abril a partir de las 13:00 horas, quedan ${diff.getSeconds.toString} segundos (${days.toString} dias y ${hours.toString} horas) para el cumpleaños.
               |Debéis traer ganas de comer :hamburger: :pizza: :shallow_pan_of_food:, beber :cocktail: :wine_glass: :tropical_drink: :beer: y pasarlo bien :tada: :confetti_ball: :balloon:.
               |Escribe /help para ver todo lo que puedo hacer.
""".stripMargin.emojize
  }

  def helpText()(implicit name: String) =
    s"""
       |Hola $name, soy MigueBot (una aplicación móvil diseñada para la fiesta) y ésto es lo que puedo hacer:
       |1) Si escribes /miguefoto recibirás una foto de Miguel Ángel!
       |2) Si escribes /reglas te daré información útil para la fiesta.
       |3) Si me mandas una foto durante la fiesta se la reenviaré a todos los usuarios del bot! Ideal para que todos las tengamos!
       |4) Si quieres que me dirija a ti de otra manera escribe /nombre.
       |5) Y otras sorpresas durante la fiesta...
       |6) Ah! Ésto no es como un grupo de whatsapp, los mensajes que escribas aquí no los recibe nadie! (Excepto las fotos como ya te he comentado antes! :wink:
     """.stripMargin.emojize

  val reglasText =
    """
      |Las reglas de la fiesta son las siguientes:
      |1) Os recibiremos a partir de las :clock1: 13:00. A las :clock130: 13:30 llegará Miguel. Sed puntuales.
      |2) Miguel no puede encontrarse nuestros coches al llegar. Por favor, evita aparcar (:car::parking::no_entry_sign:) en la calle Soria y en Picos de Urbión. Busca aparcamiento por la calle Guadalajara, Segovia o Ávila.
      |3) Para que todos nos lo pasemos bien, la política y las elecciones :ballot: son temas de conversación que podemos guardar para otra ocasión.
    """.stripMargin.emojize

  def unkownCmdText: String = Seq(
    "No conozco ese comando... :cry:" ,
    "Ahí me has pillado... xD :flushed:",
    "No se de qué me hablas...",
    "No te entiendo, lo siento..."
  ).chooseRandomStr().emojize

  val quizzNotStartedYet = "El Q60 no ha empezado todavía...".emojize

  val rootCmdOnlyText = Seq(
    "Comando solo permitido para el administrador.",
    "No has visto que ponía RESTRICTED??"
  ).chooseRandomStr().emojize

  val unkownQuizzAnswer = "La respuesta tiene que ser A,B,C o D :sob:".emojize

  def questionAlreadyAnswered(implicit name: String) = Seq(
    s"$name, tramposill@, ya has contestado a la pregunta... :disappointed: ",
    s"Uy, uy, uy, uy.... Qué trampos@ es $name..., :astonished: Solo se puede contestar una vez..."
  ).chooseRandomStr().emojize

  def questionNotAnswered(implicit name: String) = Seq(
    s"$name!!!No has respondido la pregunta... Hay que se más rápido la próxima vez...",
    s"¿Qué ha pasado $name?Pero si no has respondido!! Elige al azar aunque sea, tienes un 25% de posibilidades de acertar! :dart:"
  ).chooseRandomStr().emojize

  def questionAnsweredOK (implicit name: String)= Seq(
    "¡Respuesta correcta! ¡¡Enhorabuena!! :upside_down:",
    "¡Has acertado! ¡Eres estupend@! :tada: :tada: :tada: :tada: :confetti_ball: :confetti_ball: :confetti_ball: :confetti_ball:",
    "¡Has dado en el clavo! Se nota que conoces muy bien a Miguel."
  ).chooseRandomStr().emojize

  def questionAnsweredKO(implicit name: String) = Seq(
    s"Respuesta incorrecta. Ohhh... La próxima seguro que aciertas $name... :crying_cat_face:",
    s"No has acertado. Vaya :cry:, ánimo $name que ésta era difícil!",
    s"Uishh, casi. A ver si te tomas unas cañas con Miguel para que te cuente batallitas."
  ).chooseRandomStr().emojize

  def answerReceived = Seq(
    "Respuesta recibida!",
    "Ok!",
    "Claro, guardando la respuesta... :gear: :computer:",
    "Apuntado queda!"
  ).chooseRandomStr().emojize

  def correctAnswerText(q: Question) = {
    s"La respuesta correcta era: ${q.solution}"
  }

  def questionResultsText(goodA:Int,badA:Int) = {
    if( badA > 0.7*(goodA + badA)){
      s"""
         |Pregunta boooooooooooomba
         |:bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb:
         |Aciertos: $goodA
         |Fallos: $badA
         |:bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb::bomb:
       """.stripMargin.emojize
    }else{
      s"""
         |Aciertos: $goodA
         |Fallos: $badA
     """.stripMargin
    }

  }

  val noQuestionRightNow = "Espera un poco a qué te haga una pregunta... :wink:".emojize

  val cannotChangeNameRightNow = "Ahora mismo no puedes cambiar tu nombre en la clasificación!".emojize

  val requestNewName = "Dime como quieres llamarte en la clasificación.".emojize

  def changeNameAccepted = Seq(
    "Perfecto! Cambio tu nombre.",
    "Entendido! A partir de ahora te llamaré así.",
    "Muy bien. Me lo apunto en un post-it!",
    "Ok. Ya no se me olvida!"
  ).chooseRandomStr().emojize

  val answersKeyboard = ReplyKeyboardMarkup(Seq(
    Seq(KeyboardButton("A"),KeyboardButton("B"))
    ,Seq(KeyboardButton("C"),KeyboardButton("D")))
    ,oneTimeKeyboard = Some(true),resizeKeyboard = Some(true))

  val removeKeyboard = ReplyKeyboardRemove(true)

  val remainingTime = "Tiempo restante"

  def getPositionText(position:Int)= {

    position match {
      case 1 => ":first_place_medal:".emojize
      case 2 => ":second_place_medal:".emojize
      case 3 => ":third_place_medal:".emojize
      case n if n < 10 => position.toString+" "
      case _ => position.toString
    }
  }

  def userListText(namesL:Seq[String]) = {
    val users = namesL.zipWithIndex.map{ case (s,ind) => (ind+1).toString+") "+s}.mkString("\n")
    s"""Los usuarios registrados son:\n$users
    """.emojize
  }

  def getResultsText(position: Int, userR: UserResult) = {
    s"${getPositionText(position)}- ${userR.points.toString} puntos - ${userR.user.displayName}"
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
