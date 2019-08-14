package de.bachelorarbeit.logging

import java.text.SimpleDateFormat
import java.util.Date

import de.bachelorarbeit.logging.LogLevel.LogLevel

object Logger {
  private final val DATE_FORMAT = "yyyy-MM-dd HH:mm:ss.SSS"
  private var logLevel: LogLevel = LogLevel.INFO

  def setLogLevel(logLevel: LogLevel): Unit = {
    this.logLevel = logLevel
  }
  def getLogLevel: LogLevel = logLevel

  def log(text: String, logLevel: LogLevel): Unit = {
    if (logLevel < this.logLevel)
      return

    val timeStamp = new SimpleDateFormat(DATE_FORMAT).format(new Date())
    println(s"$timeStamp - $logLevel: $text")
  }

  def fine(obj: Any): Unit = {
    log(obj.toString, LogLevel.FINE)
  }

  def info(obj: Any): Unit = {
    log(obj.toString, LogLevel.INFO)
  }

  def warning(obj: Any): Unit = {
    log(obj.toString, LogLevel.WARNING)
  }

  def severe(obj: Any): Unit = {
    log(obj.toString, LogLevel.SEVERE)
  }
}
