package models

import org.scalatest.FunSuite
import org.scalatest.Matchers

class ExpenseGroupTest extends FunSuite with Matchers {

  val brian = Participant("Brian")
  val freshDave = Participant("Fresh Dave")
  val ben = Participant("Ben")
  val linc = Participant("Linc")
  val smokey = Participant("Smokey")

  test ("Single payment split between two people") {
    val participant1 = Participant("one", Seq(Expense(14.00, "Expense")))
    val participant2 = Participant("two")

    val expenseGroup = ExpenseGroup(Seq(participant1, participant2))

    val payments = expenseGroup.generatePayments
    val generatedPayment = new Payment(participant2, 7.00, participant1)
    assert(payments.size === 1)
    assert(payments.head === generatedPayment)

    val newPayments: Seq[Payment] = expenseGroup.calculatePayments
    assert(newPayments.size === 1)
    assert(newPayments.head === generatedPayment)
  }

  test("Two expenses split between two people") {
    val participant1 = Participant("one", Seq(Expense(19.50, "first"),
                                              Expense(20.50, "second")))
    val participant2 = Participant("two")

    val expenseGroup = ExpenseGroup(Seq(participant1, participant2))

    val payments = expenseGroup.generatePayments
    val generatedPayment = new Payment(participant2, 20.00, participant1)
    assert(payments.size === 1)
    assert(payments.head === generatedPayment)

    val newPayments: Seq[Payment] = expenseGroup.calculatePayments
    assert(newPayments.size === 1)
    assert(newPayments.head === generatedPayment)
  }

  test ("When spending is equal, no payments should be generated") {
    val expenseGroup = ExpenseGroup(Seq(
      Participant("one", Seq(Expense(10.00, "participant one expense"))),
      Participant("two", Seq(Expense(10.00, "participant two expense")))
    ))

    assert(expenseGroup.generatePayments.isEmpty)

    assert(expenseGroup.calculatePayments.isEmpty)
  }

  test("Unbalanced payments between two Participants should be reconciled") {
    val participant1 = Participant("one", Seq(Expense(5.10, "participant one expense")))
    val participant2 = Participant("two", Seq(Expense(10.00, "participant two expense")))
    val expenseGroup = ExpenseGroup(Seq(participant1, participant2))

    val payments = expenseGroup.generatePayments
    val reconciledAmount = (10.00 / 2) - (5.10 / 2)
    val generatedPayment = new Payment(participant2, reconciledAmount, participant1) // the payer and payee are reversed
    assert(payments.size === 1)
    assert(payments.head === generatedPayment)

    val newPayments = expenseGroup.calculatePayments
    val correctlyGeneratedPayment = new Payment(participant1, reconciledAmount, participant2)
    assert(newPayments.size === 1)
    assert(newPayments.head === correctlyGeneratedPayment)
  }

  test("Matching debtors to debt owners with first sample") {
    val debtors = Map[Participant, BigDecimal](brian -> 13.69, ben -> 8.98)
    val debtOwners = Map[Participant, BigDecimal](freshDave -> 22.67)

    val payments: Seq[Payment] = ExpenseGroup.matchDebtorsToDebtOwners(debtors, debtOwners)
    assert(payments.size === 2)
    payments should contain allOf (Payment(ben, 8.98, freshDave), Payment(brian, 13.69, freshDave))
  }

  ignore("Matching debtors to debt owners with second sample") {
    val debtors = Map[Participant, BigDecimal](smokey -> 42.255)
    val debtOwners = Map[Participant, BigDecimal](brian -> 0.395, freshDave -> 36.755, ben -> 5.105)

    val payments =  ExpenseGroup.matchDebtorsToDebtOwners(debtors, debtOwners)

    assert(payments.size === 3, payments.mkString(", "))
  }

  test("Payments are reconciled across multiple ExpenseGroups") {
    val expenseGroupOne = ExpenseGroup(Seq(
      Participant("one", Seq(Expense(10.00, "participant one expense"))),
      Participant("two")
    ))

    val expenseGroupTwo = ExpenseGroup(Seq(
      Participant("one"),
      Participant("two", Seq(Expense(10.00, "participant two expense")))
    ))

    val payments = ExpenseGroup.generatePayments(expenseGroupOne, expenseGroupTwo)
    assert(payments.isEmpty)
  }

  test("total expenses") {
    val bd = ExpenseGroup.totalExpenses(Seq(Expense(10, "exp"), Expense(10, "exp"), Expense(2, "exp"), Expense(3, "exp")))
    assert(BigDecimal(25) === bd)
  }

}
