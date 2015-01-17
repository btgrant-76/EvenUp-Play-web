package models

import org.scalatest.FunSuite

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

}
