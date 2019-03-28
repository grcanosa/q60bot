package com.grcanosa.q60bot.bot

import java.io.File
import java.time.LocalDateTime
import java.time.Duration

import com.vdurmont.emoji.EmojiParser

import scala.util.Random


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


  def getPhotoPath():String = {
    var d = new File("photos")
    var l = d.listFiles()
    l(Random.nextInt(l.length)).getAbsolutePath
  }
}
