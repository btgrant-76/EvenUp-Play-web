package models

import Payment.toRoundedPayment

case class ExpenseGroup(participants: Seq[Participant]) {

  @Deprecated
  def generatePayments = {

    val totalExpendituresForEachParticipant = participants.map { participant =>
      (participant, participant.expenses.map(_.amount).sum)
    }.filterNot(_._2 <= 0).toMap

    val payments = totalExpendituresForEachParticipant.flatMap { participantAndSpending =>
      val splitPayment = participantAndSpending._2 / participants.size

      participants.filterNot(p => p == participantAndSpending._1)
        .map { p => new Payment(p, splitPayment, participantAndSpending._1)}
    }.toSeq

    ExpenseGroup.reconcilePayments(payments)
  }

  def calculatePayments = {
    val spendingByParticipant: Map[Participant, BigDecimal] = participants.map { p =>
      (p, p.expenses.map {_.amount}.sum)
    }.toMap

    val totalPayments: BigDecimal = spendingByParticipant.values.sum
    val costPerPerson: BigDecimal = totalPayments / participants.size

    val spendingLessCost: Map[Participant, BigDecimal] = spendingByParticipant.map { entry: (Participant, BigDecimal) =>
      (entry._1, entry._2 - costPerPerson)
    }

    val debtByDebtors: Map[Participant, BigDecimal] = spendingLessCost.filter(byNegativeAmounts)
      .map {entry: (Participant, BigDecimal) => (entry._1, entry._2 * -1)
    }

    val debtByDebtOwners: Map[Participant, BigDecimal] = spendingLessCost.filterNot(byNegativeAmounts)

    ExpenseGroup.matchDebtorsToDebtOwners(debtByDebtors, debtByDebtOwners)
  }

  // TODO return a Set instead of a Seq
  private def byNegativeAmounts(participantAndAmount: (Participant, BigDecimal)) = participantAndAmount._2 <= 0
}

object ExpenseGroup {

  val ZERO = BigDecimal(0)

  def matchDebtorsToDebtOwners(debtors: Map[Participant, BigDecimal], debtOwners: Map[Participant, BigDecimal]): Seq[Payment] = {

    def accPay(payments: Seq[Payment], debtors: Map[Participant, BigDecimal], debtOwners: Map[Participant, BigDecimal]): Seq[Payment] = {
      if (debtors.isEmpty || debtOwners.isEmpty) {
        if (debtOwners.nonEmpty) {
          debtOwners.foreach { debtOwner => assert(debtOwner._2 < .01, debtOwner)}
        }

        if (debtors.nonEmpty) {
          debtors.foreach { debtor => assert(debtor._2 < .01, debtor)}
        }
        payments.map(toRoundedPayment)
      } else {
        var debt: (Participant, BigDecimal) = debtors.head
        var debtOwnersToUpdate = Map[Participant, BigDecimal]() ++ debtOwners
        var addPaymentsHere = payments

        for (ownedDebt: (Participant, BigDecimal) <- debtOwners) {
          if (debt._2 == ZERO) {
            // do nothing; the debt was resolved on a previous pass
          } else if (debt._2.equals(ownedDebt._2)) {
            addPaymentsHere = addPaymentsHere :+ Payment(debt._1, ownedDebt._2, ownedDebt._1)
            debt = (debt._1, 0)
            debtOwnersToUpdate = debtOwnersToUpdate - ownedDebt._1
          } else if (debt._2 > ownedDebt._2) {
            addPaymentsHere = addPaymentsHere :+ Payment(debt._1, ownedDebt._2, ownedDebt._1)
            debt = (debt._1, debt._2 - ownedDebt._2)
            debtOwnersToUpdate = debtOwnersToUpdate - ownedDebt._1 // they're paid off
          } else { // debt._2 < ownedDebt._2
            val payment = Payment(debt._1, debt._2, ownedDebt._1)
            addPaymentsHere = addPaymentsHere :+ payment
            val remainderOfOwnedDebt = ownedDebt._2 - debt._2

            assert(remainderOfOwnedDebt > 0)
            debtOwnersToUpdate = ( debtOwnersToUpdate - ownedDebt._1) + (ownedDebt._1 -> remainderOfOwnedDebt)
            debt = (debt._1, 0) // the debtor has paid their debt
          }
        }

        val debtsPostPayments = if (debt._2 == ZERO) {
          debtors - debt._1 // a debtor no more!
        } else {
          debtors - debt._1 + (debt._1 -> debt._2)
        }

        accPay(addPaymentsHere, debtsPostPayments, debtOwnersToUpdate)
      }
    }

    accPay(Seq(), debtors, debtOwners)
  }

  private def reconcilePayments(payments: Traversable[Payment]): Seq[Payment] = {
    payments.foldLeft(Set[Payment]()) {(reconciled, curPayment) =>
      payments.find { p =>
        p.from == curPayment.to && p.to == curPayment.from
      }.map { mirroredPayment =>

        val paymentDifference: BigDecimal = mirroredPayment.amount - curPayment.amount

        if (paymentDifference equals ZERO) {
          // the two payments cancel each other out
          reconciled
        } else {
          reconciled + {
            if (paymentDifference < 0) {
              new Payment(mirroredPayment.from, paymentDifference.abs, mirroredPayment.to)
            } else {
              new Payment(mirroredPayment.to, paymentDifference, mirroredPayment.from)
            }
          }
        }
      }.getOrElse(reconciled + curPayment)
    }.toSeq
  }

  def generatePayments(expenseGroups: ExpenseGroup*): Seq[Payment] = {
    reconcilePayments(expenseGroups.foldLeft(Seq[Payment]()) {(payments: Seq[Payment], group: ExpenseGroup) =>
      payments ++ group.generatePayments
    })
  }

  def totalExpenses(expenses: Traversable[Expense]): BigDecimal = {
    expenses.map(_.amount).sum
  }

}
