package models

import play.api.cache.Cache
import play.api.Play.current

case class Participant(name: String)

object Participant {
  val SESSION_KEY = "session.participants"

  private val emptySeq = Seq[Participant]()

  def getParticipantsFromCache(session: play.api.mvc.Session) = {
    session.get(SESSION_KEY).map { value =>
      Cache.getOrElse(value)(emptySeq)
    }.getOrElse(emptySeq)
  }
}
