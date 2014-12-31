package controllers

import models.Participant
import play.api.mvc.{Controller, Action}

object Expenses extends Controller {

  val DEV_PARTICIPANTS = Seq(new Participant("Goofus"), new Participant("Gallant"))

  private val AMOUNT_FIELD = "amount"
  private val PARTICIPANT_FIELD = "participant_name"

  def index = Action { request =>
    val participants =  DEV_PARTICIPANTS // Participant.getParticipantsFromCache(request.session)

    if (participants.isEmpty) {
      Redirect(routes.Participants.loadParticipantListPage)
    } else {
      Ok(views.html.expense_entry(participants, Map()))
    }
  }

  def addExpense = Action { request =>
    require(request.body.asFormUrlEncoded.isDefined)

    println(s"formUrlEncoded = ${request.body.asFormUrlEncoded}")
    val amount: BigDecimal = request.body.asFormUrlEncoded.get(AMOUNT_FIELD).head.toDouble
    val participantName: String = request.body.asFormUrlEncoded.get(PARTICIPANT_FIELD).head

    val matchingParticipant = DEV_PARTICIPANTS
      .collectFirst ({ case p: Participant if p.name == participantName => p })

    if (matchingParticipant.isEmpty) {
      NotFound(s"A Participant named $participantName was not found")
    } else {
      //    Ok(s"added $amount for $participantName")
      Ok(views.html.expense_entry(DEV_PARTICIPANTS, Map()))
    }
  }

}
