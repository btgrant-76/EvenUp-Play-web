package controllers

import play.api.cache.Cache
import play.api.Logger
import play.api.Play.current
import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc.{Session, Action, Controller}

object Participants extends Controller {

  case class Participant(name: String)

  private val participantForm: Form[Participant] = Form(
    mapping(
      "name" -> nonEmptyText
    )(Participant.apply)(Participant.unapply)
  )

  def loadParticipantListPage = Action { implicit request =>
    Logger.info("called loadParticipantListPage")

    val stuffs: Seq[Participant] = if (request.session.get("foo").isDefined) {
      Cache.getOrElse("bar") { Seq[Participant]() }
    } else {
      Logger.info("session.foo is undefined")
      Seq()
    }

    Ok(views.html.view_participant_names(stuffs, "tracker"))
  }

  def addParticipant = Action { implicit request =>
//    play.cache.Cache.
    Logger.info("called addParticipant")

    val boundForm = participantForm.bindFromRequest

    if (boundForm.hasErrors) {
      BadRequest(boundForm.errors.mkString("\n"))
    } else {

      val newParticipant: Participant = boundForm.get

      val participantsKey: String = if (request.session.isEmpty) {
        request.session + ("foo" -> "bar")
        "bar"
      } else {
        request.session.get("foo")
      }.get // TODO is this safe?

      val participants: Seq[Participant] = Cache.getOrElse(participantsKey) { Seq[Participant]() }
      val newParticipants: Seq[Participant] =  participants :+ newParticipant
      Cache.set(participantsKey, newParticipants)

      Ok(views.html.view_participant_names(newParticipants, "tracker"))
    }

  }

}
