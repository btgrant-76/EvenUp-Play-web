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

  case class NewParticipant(name: String)

  val participantForm: Form[NewParticipant] = Form(
    mapping(
      "name" -> nonEmptyText
    )(NewParticipant.apply)(NewParticipant.unapply)
  )

  def loadParticipantListPage = Action { implicit request =>
    Logger.info("called loadParticipantListPage")

    val participantsFromCache = Participant.getParticipantsFromCache(request.session)

    Ok(views.html.view_participant_names(participantsFromCache))
  }

  def addParticipants = Action { implicit request =>
    Logger.info("called addParticipants")
    
    // /participants?name=Brian%20Grant&name=Fresh%20Dave&name=Ben%20Beckstrom&name=Linc%20Abbey
    if (request.queryString.get("name").isDefined) {
      handleNames(request.queryString) // dev options only
    } else {
      request.body.asFormUrlEncoded
        .map { handleNames }
        .orElse { Some(BadRequest("Missing form body")) }
        .get
    }
  }

  private def handleNames(namesMap: Map[String, Seq[String]]) = {
    if (namesMap.get("name").isEmpty) {
      BadRequest("No names were submitted")
    } else {
      val names = namesMap.get("name").get
      // Ok("Participants Added:  " + names.mkString(", "))
      Ok(views.html.expense_entry(
        names.map { new Participant(_)},
        Map()))
    }
  }

  def addParticipant = Action { implicit request =>
    Logger.info("called addParticipant")

    val boundForm = participantForm.bindFromRequest

    if (boundForm.hasErrors) {
      BadRequest(boundForm.errors.mkString("\n"))
    } else {

      val newParticipant: Participant = new Participant(boundForm.get.name)

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

      Ok(views.html.view_participant_names(newParticipants))
        .withSession(request.session + (Participant.SESSION_KEY, participantsKey)) // TODO redundant?
    }
  }

}
