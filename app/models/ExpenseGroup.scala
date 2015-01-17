package models

case class ExpenseGroup(participants: Seq[Participant]) {

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

}
