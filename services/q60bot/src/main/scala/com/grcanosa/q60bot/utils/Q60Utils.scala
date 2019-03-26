package com.grcanosa.q60bot.utils

import scala.io.{BufferedSource, Source}
import com.grcanosa.q60bot.model.{Opcion, Question}
import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.LoggerFactory

import scala.util.Try


object Q60Utils {

  val mylog  = LoggerFactory.getLogger("q60logger")

  lazy val config: Config = {
    ConfigFactory.load("application.conf")
  }

  lazy val botToken = config.getString("bot.token")

  lazy val rootId = config.getLong("bot.rootId")

  def loadQuestions() = {
    val bufferedSource: BufferedSource = scala.io.Source.fromFile("src/main/resources/preguntas.csv")
    bufferedSource.getLines().flatMap{
      l =>
        mylog.info(l)
        val spli: Array[String] = l.split('|').map(_.trim)
        spli.foreach(mylog.info)
        if(spli.size > 2){
          Try {
            mylog.info("inside try")
            val question = spli(0)
            mylog.info(s"question $question")
            val points = spli(1).toInt
            mylog.info(s"points ${points.toString}")
            val opciones: Array[Opcion] =
              for (respuesta <- spli.drop(2)) yield {
                mylog.info(s"Respuesta: $respuesta")
                if(respuesta.startsWith("TRUE:")){
                  Opcion(respuesta.replace("TRUE:",""),true)
                }else{
                  Opcion(respuesta,false)
                }
              }
            mylog.info("Returning question")
            Some(Question(question,opciones.toSeq,points))
          }.getOrElse(None)
        }else{
          mylog.warn(s"Line $l has less than 3 fields")
          None
        }
    }

  }

  lazy val allQuestions: Seq[Question] = loadQuestions().toSeq

}
