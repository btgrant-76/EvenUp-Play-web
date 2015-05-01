package models

import org.scalatest.FunSuite
import org.scalatest.Matchers

class ExpenseGroupTest extends FunSuite with Matchers {

  val brian = Participant("Brian")
  val freshDave = Participant("Fresh Dave")
  val ben = Participant("Ben")
  val linc = Participant("Linc")
  val smoky = Participant("Smoky")
  val sten = Participant("Sten")
  val ringo = Participant("Ringo")

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
    val reconciledAmount = 2.45
    val generatedPayment = new Payment(participant2, reconciledAmount, participant1) // the payer and payee are reversed
    assert(payments.size === 1)
    assert(payments.head === generatedPayment)

    val newPayments = expenseGroup.calculatePayments
    val correctlyGeneratedPayment = new Payment(participant1, reconciledAmount, participant2)
    assert(newPayments.size === 1)
    assert(newPayments.head === correctlyGeneratedPayment)
    newPayments.foreach {paymentsMustAlwaysBePositive}
  }

  test("Matching debtors to debt owners with first sample") {
    val debtors = Map[Participant, BigDecimal](brian -> 13.69, ben -> 8.98)
    val debtOwners = Map[Participant, BigDecimal](freshDave -> 22.67)

    val payments: Seq[Payment] = ExpenseGroup.matchDebtorsToDebtOwners(debtors, debtOwners)
    assert(payments.size === 2)
    payments should contain allOf (Payment(ben, 8.98, freshDave), Payment(brian, 13.69, freshDave))
    payments.foreach {paymentsMustAlwaysBePositive}
  }

  test("Matching debtors to debt owners with second sample") {
    val debtors = Map[Participant, BigDecimal](smoky -> 42.255)
    val debtOwners = Map[Participant, BigDecimal](brian -> 0.395, freshDave -> 36.755, ben -> 5.105)

    val payments =  ExpenseGroup.matchDebtorsToDebtOwners(debtors, debtOwners)

    val paymentsString = payments.mkString(", ")
    assert(payments.size === 3, paymentsString)

    payments.foreach {paymentsMustAlwaysBePositive}

    payments should contain allOf (Payment(smoky, 0.40, brian), Payment(smoky, 36.76, freshDave),
                                  Payment(smoky, 5.11, ben))
  }

  test("No zero payments") {
    val allParticipantsWithExpenses = Seq(
      Participant("one", Seq(Expense(58.35, "one one"), Expense(23.52, "one two"), Expense(50, "one three"))),
      Participant("two", Seq(Expense(50, "two one"))),
      Participant("three", Seq(Expense(68.75, "three one"))),
      Participant("four", Seq(Expense(80, "four one"))),
      Participant("five", Seq(Expense(26.67, "five one"), Expense(37.75, "five two"), Expense(19.36, "five three"))),
      Participant("six", Seq(Expense(75, "five one"))),
      Participant("seven"))

    val payments = ExpenseGroup(allParticipantsWithExpenses).calculatePayments

    payments.foreach {paymentsMustAlwaysBePositive}
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

  def paymentsMustAlwaysBePositive(p: Payment): Unit = {
    assert(p.amount > 0)
  }

}
