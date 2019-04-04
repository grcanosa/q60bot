package com.grcanosa.q60bot.utils


import com.typesafe.config.{Config, ConfigFactory}
import org.slf4j.LoggerFactory


object Q60Utils {

  val mylog  = LoggerFactory.getLogger("q60logger")

  lazy val config: Config = {
    ConfigFactory.load("application.conf")
  }




}
