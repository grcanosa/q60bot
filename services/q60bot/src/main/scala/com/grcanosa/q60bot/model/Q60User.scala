package com.grcanosa.q60bot.model


case class Q60User(chatId: Long, firstName: Option[String], lastName: Option[String], username: Option[String]){
  lazy val displayName: String = {
    if(firstName.isDefined || lastName.isDefined){
      s"${firstName.getOrElse("")} ${lastName.getOrElse("")}"
    }else if(username.isDefined){
      s"${username.get}"
    }else{
      chatId.toString
    }
  }

}
