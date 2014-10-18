package controllers

import java.util.UUID

import play.api.Logger
import play.api.Play.current
import play.api.cache.Cache
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, Controller}


object Participants extends Controller {

  val SESSION_PARTICIPANTS_KEY = "session.participants"

  case class Participant(name: String)

  val participantForm: Form[Participant] = Form(
    mapping(
      "name" -> nonEmptyText
    )(Participant.apply)(Participant.unapply)
  )

  def loadParticipantListPage = Action { implicit request =>
    Logger.info("called loadParticipantListPage")

    val participantsFromCache = request.session.get(SESSION_PARTICIPANTS_KEY)
      .map { value =>
        println(s"$SESSION_PARTICIPANTS_KEY -> $value")
        Cache.getOrElse(value){ Seq[Participant]() }
      }


    Ok(views.html.view_participant_names(participantsFromCache.getOrElse(Seq()), participantForm))
  }

  def addParticipant = Action { implicit request =>
    Logger.info("called addParticipant")

    val boundForm = participantForm.bindFromRequest

    if (boundForm.hasErrors) {
      BadRequest(boundForm.errors.mkString("\n"))
    } else {

      val newParticipant: Participant = boundForm.get

      val participantsKey: String = if (request.session.isEmpty) {
        val participantsCacheKey = UUID.randomUUID().toString
        request.session + (SESSION_PARTICIPANTS_KEY -> participantsCacheKey)
        participantsCacheKey
      } else {
        request.session.get(SESSION_PARTICIPANTS_KEY)
      }.get // TODO is this safe?

      val participants: Seq[Participant] = Cache.getOrElse(participantsKey) { Seq[Participant]() }
      val newParticipants: Seq[Participant] =  participants :+ newParticipant
      Cache.set(participantsKey, newParticipants)

      Ok(views.html.view_participant_names(newParticipants, participantForm))
        .withSession(request.session + (SESSION_PARTICIPANTS_KEY, participantsKey)) // TODO redundant?
    }
  }

}
