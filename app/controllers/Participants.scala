package controllers

import java.util.UUID

import models.Participant
import play.api.Logger
import play.api.Play.current
import play.api.cache.Cache
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Action, Controller}


object Participants extends Controller {

  val participantForm: Form[Participant] = Form(
    mapping(
      "name" -> nonEmptyText
    )(Participant.apply)(Participant.unapply)
  )

  def loadParticipantListPage = Action { implicit request =>
    Logger.info("called loadParticipantListPage")

    val participantsFromCache = Participant.getParticipantsFromCache(request.session)

    Ok(views.html.view_participant_names(participantsFromCache, participantForm))
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
        request.session + (Participant.SESSION_KEY -> participantsCacheKey)
        participantsCacheKey
      } else {
        request.session.get(Participant.SESSION_KEY)
      }.get // TODO is this safe?

      val participants: Seq[Participant] = // Cache.getOrElse(participantsKey) { Seq[Participant]() }
        Participant.getParticipantsFromCache(request.session)
      val newParticipants: Seq[Participant] =  (participants :+ newParticipant)
        .sortWith {(p1: Participant, p2: Participant) => p1.name < p2.name }
      Cache.set(participantsKey, newParticipants)

      Ok(views.html.view_participant_names(newParticipants, participantForm))
        .withSession(request.session + (Participant.SESSION_KEY, participantsKey)) // TODO redundant?
    }
  }

}
