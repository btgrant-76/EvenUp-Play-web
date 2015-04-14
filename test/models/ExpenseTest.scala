package models

import models.Expense.jsonReads
import org.scalatest.FunSuite
import play.api.libs.json._

class ExpenseTest extends FunSuite {

  test("An Expense cannot have an amount of 0") {
    intercept[IllegalArgumentException] {
      new Expense(0, "description")
    }
  }

  test("An Expense cannot have a negative amount") {
    intercept[IllegalArgumentException] {
      new Expense(-5.32, "description")
    }
}

  test("An Expense cannot have an empty description") {
    intercept[IllegalArgumentException] {
      new Expense(5.32, "")
    }
  }

  test("JSON is parsed to Expense instances") {
    val singleExpense = Json.parse("""{"name" : "Maker's Mark", "amount" : 22.44}""")
    val exp: Expense = singleExpense.as[Expense] // implicit (jsonReads)

    assert(exp.name === "Maker's Mark")
    assert(exp.amount === BigDecimal("22.44"))
  }

  test("JSON array is parsed as Seq[Expense]") {
    val expenseArray = Json.parse(
      """[{"name" : "Maker's Mark", "amount" : 22.44}, {"name" : "Two Brothers Variety Pack", "amount" : 25.92}]""")
    val expenses: Seq[Expense] = expenseArray.as[Seq[Expense]]

    expenses.foldLeft(0) {(index: Int, exp: Expense) =>
      index match {
        case 0 =>
          assert(exp.name === "Maker's Mark")
          assert(exp.amount === BigDecimal("22.44"))
        case 1 =>
          assert(exp.name === "Two Brothers Variety Pack")
          assert(exp.amount === BigDecimal("25.92"))
        case _ => throw new AssertionError()
      }
      index + 1
    }
  }

}
