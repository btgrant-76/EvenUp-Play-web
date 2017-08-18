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

  test("Participant equality is symmetric") {
    val x = Participant("p1")
    val y = Participant("p1")

    assert(x.equals(y) && y.equals(x))
    assert(x.hashCode() === y.hashCode() && y.hashCode() === x.hashCode())
  }

  test("Participant equality is reflexive") {
    val x = Participant("p1")

    assert(x.equals(x))
    assert(x.hashCode() === x.hashCode())
  }

  test("Participant equality is transitive") {
    val x = Participant("p1")
    val y = Participant("p1")
    val z = Participant("p1")

    assert(x.equals(y) && y.equals(z) && z.equals(x))
    assert(x.hashCode() === y.hashCode() && y.hashCode() === z.hashCode() && z.hashCode() === x.hashCode())
  }

  test("A participant's name may not be null") {
    intercept[IllegalArgumentException] {
      new Participant(name = null)
    }
  }

  test("A participant's name may not be empty") {
    intercept[IllegalArgumentException] {
      new Participant(name = "")
    }
  }

  test("A participant's name may not be blank") {
    intercept[IllegalArgumentException] {
      new Participant(name = "   ")
    }
  }

  test("A participant's expenses may not be null") {
    intercept[IllegalArgumentException] {
      new Participant("foo", null)
    }
  }

}
