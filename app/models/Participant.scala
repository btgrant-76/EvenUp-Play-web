package models

import org.apache.commons.lang3.StringUtils
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.cache.Cache
import play.api.Play.current

case class Participant(name: String, expenses: Seq[Expense] = Seq()) {
  require(StringUtils.isNotBlank(name) && expenses != null)

  lazy val toStringValue = s"${this.getClass.getName}{name : $name}"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Participant]

  override def equals(other: Any): Boolean = other match {
    case that: Participant =>
      (that canEqual this) &&
      name == that.name
    case _ => false
  }

  override def hashCode(): Int = name.hashCode

  override def toString: String = toStringValue

}

object Participant {
  val SESSION_KEY = "session.participants"

  private val emptySeq = Seq[Participant]()

  def getParticipantsFromCache(session: play.api.mvc.Session) = {
    session.get(SESSION_KEY).map { value =>
      Cache.getOrElse(value)(emptySeq)
    }.getOrElse(emptySeq)
  }

  implicit val participantRead: Reads[Participant] = (
    (JsPath \ "name").read[String] and
    (JsPath \ "expenses").read[Seq[Expense]]
  )(Participant.apply _)

}
