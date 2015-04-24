package controllers

import java.util.UUID

import models.{Payment, ExpenseGroup, Participant}
import play.api.libs.json.Json
import play.api.cache.Cache
import play.api.mvc.{Action, Controller}
import play.api.Play.current

object Expenses extends Controller {

  val DEV_PARTICIPANTS = Seq(new Participant("Goofus"), new Participant("Gallant"))

  val TEST_JSON = """[
                    |  { "name" : "Brian",
                    |    "expenses" : [
                    |      {"description" : "Maker's Mark",
                    |       "amount" : 22.44},
                    |      {"description" : "Two Brothers variety pack",
                    |       "amount" : 25.19}
                    |    ]
                    |  },
                    |
                    |  { "name" : "Fresh Dave",
                    |    "expenses" : [
                    |      {"description" : "Apples",
                    |       "amount" : 8.99},
                    |      {"description" : "Double Dirty Bastard",
                    |       "amount" : 75.00}
                    |    ]
                    |  },
                    |
                    |  {"name" : "Ben",
                    |    "expenses" : [
                    |      {"description" : "Frozen pizzas",
                    |       "amount" : 38.12},
                    |      {"description" : "Rabbits",
                    |       "amount" : 14.22}
                    |    ]
                    |  }
                    |]
                    |""".stripMargin

  private val AMOUNT_FIELD = "amount"
  private val PARTICIPANT_FIELD = "participant_name"

  def index = Action { request =>
    val participants =  DEV_PARTICIPANTS // Participant.getParticipantsFromCache(request.session)

    if (participants.isEmpty) {
      Redirect(routes.Participants.loadParticipantListPage())
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
      Ok(views.html.expense_entry(DEV_PARTICIPANTS, Map()))
    }
  }

  def calculateExpenses = Action(parse.json) { request =>
    val participants = request.body.as[Seq[Participant]]
    val payments = new ExpenseGroup(participants).calculatePayments

    val resourceId = UUID.randomUUID().toString
    Cache.set(resourceId, (participants, payments), 7200)

    Created.withHeaders("Location" -> s"/calculations/$resourceId")
  }

  def displayCalculations(resourceId: String) = Action {
    if ("dev" == resourceId) {
      val participantsWithExpenses = Json.parse(TEST_JSON).as[Seq[Participant]]
      val payments = new ExpenseGroup(participantsWithExpenses).calculatePayments // generatePayments

      Ok(views.html.calculations_display(participantsWithExpenses, payments))
    } else {
      val cachedValues = Cache.get(resourceId).asInstanceOf[Option[(Seq[Participant], Seq[Payment])]]
      cachedValues.map { pairOfParticipantsAndPayments =>
        Ok(views.html.calculations_display(pairOfParticipantsAndPayments._1, pairOfParticipantsAndPayments._2))
      }.getOrElse(NotFound(s"no records found for $resourceId"))
    }
  }

}
