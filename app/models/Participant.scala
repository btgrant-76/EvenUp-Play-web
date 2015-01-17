package models

import org.apache.commons.lang3.StringUtils
import play.api.cache.Cache
import play.api.Play.current

class Participant(val name: String, val expenses: Seq[Expense] = Seq()) {
  assume(StringUtils.isNotBlank(name))

  lazy val hashCodeValue = Seq(name).map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  lazy val toStringValue = s"${this.getClass.getName}{name : $name}"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Participant]

  override def equals(other: Any): Boolean = other match {
    case that: Participant =>
      (that canEqual this) &&
      name == that.name
    case _ => false
  }

  override def hashCode(): Int = hashCodeValue

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


}
