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
    mylog.info("Loading Users")
    val bufferedSource: BufferedSource = scala.io.Source.fromFile("users.csv")
    bufferedSource.getLines().map{
      l => {
        val spli: Array[String] = l.split('|').map(_.trim)
        val first = if(spli(1) != "") Some(spli(1)) else None
        val last = if(spli(2) != "") Some(spli(2)) else None
        val username = if(spli(3) != "") Some(spli(3)) else None
        val user = Q60User(spli(0).toLong,first,last,username)
        mylog.info(s"Loading: $user")
        user
      }
    }.toSeq
  }

  def saveUsers(users: Seq[Q60User]) = {
    mylog.info("Saving users")
    import java.io._
    val fil = new PrintWriter(new File("users.csv"))
    users.foreach(u => {
      val s = u.chatId.toString+"|"+u.firstName.getOrElse("")+"|"+u.lastName.getOrElse("")+"|"+u.username.getOrElse("")
      mylog.info(s"Saving: $s")
      fil.println(s)
    })
    fil.flush()
    fil.close()
  }

  lazy val allQuestions: Seq[Question] = loadQuestions().toSeq


  def getPhotoPath():String = {
    var d = new File(photoPath)
    var l = d.listFiles()
    val ind = Random.nextInt(l.length)
    l(ind).getAbsolutePath
  }
}
