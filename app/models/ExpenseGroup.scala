package models

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
    val participantCount = participants.size

    val spendingByParticipant: Map[Participant, BigDecimal] = participants.map { p =>
      (p, p.expenses.map {_.amount}.sum)
    }.toMap

    val totalPayments: BigDecimal = spendingByParticipant.values.sum
    val costPerPerson: BigDecimal = totalPayments / participants.size

    val spendingLessCost: Map[Participant, BigDecimal] = spendingByParticipant.map { entry: (Participant, BigDecimal) =>
      (entry._1, entry._2 - costPerPerson)
    }

    val debtByDebtors: Map[Participant, BigDecimal] = spendingLessCost.filter(byNegativeAmounts)
//    val debtByDebtOwners: Map[Participant, BigDecimal] = spendingLessCost.filterNot {entry: (Participant, BigDecimal) => debtByDebtors.keySet.contains(entry._1)}
    val debtByDebtOwners: Map[Participant, BigDecimal] = spendingLessCost.filterNot(byNegativeAmounts)

    println("debt by debtors:  " + debtByDebtors.mkString(", "))
    println("debt by debt owners:  " + debtByDebtOwners.mkString(", "))

    // TODO try recursively stepping through both maps and making accumulating payments as we go along.

    matchDebtorsToDebtOwners(debtByDebtors, debtByDebtOwners)
  }

  // TODO return a Set instead of a Seq
  private def matchDebtorsToDebtOwners(debtors: Map[Participant, BigDecimal], debtOwners: Map[Participant, BigDecimal]) = {

    def accumulatePayments(payments: Seq[Payment], debtors: Map[Participant, BigDecimal], debtOwners: Map[Participant, BigDecimal]): Seq[Payment] = {

      if (debtors.isEmpty && debtOwners.isEmpty) {
        println("returning payments:  " + payments.mkString(", "))
        payments
      } else {
        debtors.flatMap { debt: (Participant, BigDecimal) =>
          debtOwners.flatMap { ownedDebt: (Participant, BigDecimal) =>
            if (debt._2.equals(ownedDebt._2 * -1)) {
              println(debt + ", " + ownedDebt)
              accumulatePayments(payments :+ Payment(debt._1, ownedDebt._2, ownedDebt._1), debtors - debt._1, debtOwners - ownedDebt._1)
            } else {
              payments
            }
          }
        }.toSeq
      }
    }

    accumulatePayments(Seq(), debtors, debtOwners)
  }

  // TODO eliminate 0 values since they are neither owed nor do they owe?
  private def byNegativeAmounts(participantAndAmount: (Participant, BigDecimal)) = participantAndAmount._2 < 0

}

object ExpenseGroup {

  val ZERO = BigDecimal(0)

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
    expenses.map(_.amount).reduce((a: BigDecimal, b: BigDecimal) => a + b)
  }

}
