package com.grcanosa.q60bot.bot

import java.io.File
import scala.concurrent.duration._
import com.grcanosa.q60bot.model.{Q60User, Question}

import scala.io.BufferedSource
import scala.util.{Random, Try}

object BotData {

  import com.grcanosa.q60bot.utils.Q60Utils._

  lazy val token = config.getString("bot.token")

  lazy val rootId = config.getLong("bot.rootId")

  lazy val photoPath = config.getString("bot.photoPath")


  val questionAnswerDelay = 20 seconds

  def loadQuestions() = {
    val bufferedSource: BufferedSource = scala.io.Source.fromFile("src/main/resources/preguntas.csv")
    bufferedSource.getLines().flatMap{
      l =>
        //mylog.info(l)
        val spli: Array[String] = l.split('|').map(_.trim)
        //spli.foreach(mylog.info)
        if(spli.length > 2){
          Try {
            //mylog.info("inside try")
            val question = spli(0)
            //mylog.info(s"question $question")
            val points = spli(1).toInt
            //mylog.info(s"points ${points.toString}")
            val respA = spli(2)
            val respB = spli(3)
            val respC = spli(4)
            val respD = spli(5)
            val solution = spli(6)
            //mylog.info("Returning question")
            Some(Question(question,points,respA,respB,respC,respD,solution))
          }.getOrElse(None)
        }else{
          mylog.warn(s"Line $l has less than 3 fields")
          None
        }
    }

  }

  def loadUsers(): Seq[Q60User] = {
    Seq.empty
  }

  def saveUsers(users: Seq[Q60User]) = {
    import java.io._
    val fil = new PrintWriter(new File("users.csv"))
    users.foreach(u => {

    })
  }

  lazy val allQuestions: Seq[Question] = loadQuestions().toSeq


  def getPhotoPath():String = {
    var d = new File(photoPath)
    var l = d.listFiles()
    val ind = Random.nextInt(l.length)
    l(ind).getAbsolutePath
  }
}
