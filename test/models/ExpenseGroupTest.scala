package models

import org.scalamock.scalatest.MockFactory
import org.scalatest.FunSuite

class ExpenseGroupTest extends FunSuite with MockFactory {

  test ("Single payment split between two people") {
    val participant1 = new Participant("one", Seq(new Expense(14.00, "Expense")))
    val participant2 = new Participant("two")

    val expenseGroup = new ExpenseGroup(Seq(participant1, participant2))

    val payments = expenseGroup.generatePayments
    assert(payments.size === 1)
    assert(payments.head === new Payment(participant2, 7.00, participant1))
  }

  test("Two expenses split between two people") {
    val participant1 = new Participant("one", Seq(new Expense(19.50, "first"),
                                                  new Expense(20.50, "second")))
    val participant2 = new Participant("two")

    val expenseGroup = new ExpenseGroup(Seq(participant1, participant2))

    val payments = expenseGroup.generatePayments
    assert(payments.size === 1)
    assert(payments.head === new Payment(participant2, 20.00, participant1))
  }

  test ("When spending is equal, no payments should be generated") {
    val expenseGroup = new ExpenseGroup(Seq(
      new Participant("one", Seq(new Expense(10.00, "participant one expense"))),
      new Participant("two", Seq(new Expense(10.00, "participant two expense")))
    ))

    assert(expenseGroup.generatePayments.isEmpty)
  }

  test("Unbalanced payments between two Participants should be reconciled") {
    val participant1 = new Participant("one", Seq(new Expense(5.10, "participant one expense")))
    val participant2 = new Participant("two", Seq(new Expense(10.00, "participant two expense")))
    val expenseGroup = new ExpenseGroup(Seq(participant1, participant2))

    val payments = expenseGroup.generatePayments

    assert(payments.size === 1)

    val reconciledAmount = (10.00 / 2) - (5.10 / 2)

    assert(payments.head === new Payment(participant2, reconciledAmount, participant1))
  }

  test("Payments are reconciled across multiple ExpenseGroups") {
    val expenseGroupOne = new ExpenseGroup(Seq(
      new Participant("one", Seq(new Expense(10.00, "participant one expense"))),
      new Participant("two")
    ))

    val expenseGroupTwo = new ExpenseGroup(Seq(
      new Participant("one"),
      new Participant("two", Seq(new Expense(10.00, "participant two expense")))
    ))

    val payments = ExpenseGroup.generatePayments(expenseGroupOne, expenseGroupTwo)
    assert(payments.isEmpty)
  }

}
