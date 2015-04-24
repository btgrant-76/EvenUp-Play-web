package models

import org.scalatest.FunSuite
import play.api.libs.json._

class ParticipantTest extends FunSuite {

  val participantWithNoExpenses = """{"name" : "Mark Maker", "expenses" : []}"""
  val participantWithExpenses = """{"name" : "Mark Maker", "expenses" : [{"description" : "Maker's Mark", "amount" : 22.44}, {"description" : "Two Brothers Variety Pack", "amount" : 25.92}]}"""

  test("JSON is parsed to a Participant without expenses") {
    val participant: Participant = Json.parse(participantWithNoExpenses).as[Participant]

    assert(participant.name === "Mark Maker")
    assert(participant.expenses === Seq())
  }

  test("JSON is parsed to a Participant with Expenses") {
    val participant: Participant = Json.parse(participantWithExpenses).as[Participant]
    assert(participant.name === "Mark Maker")

    participant.expenses.foldLeft(0) {(index: Int, exp: Expense) =>
      index match {
        case 0 =>
          assert(exp.description === "Maker's Mark")
          assert(exp.amount === BigDecimal("22.44"))
        case 1 =>
          assert(exp.description === "Two Brothers Variety Pack")
          assert(exp.amount === BigDecimal("25.92"))
        case _ => throw new AssertionError()
      }
      index + 1
    }
  }

}
