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
      .map {entry: (Participant, BigDecimal) => (entry._1, entry._2 * -1)
    }

//    val debtByDebtOwners: Map[Participant, BigDecimal] = spendingLessCost.filterNot {entry: (Participant, BigDecimal) => debtByDebtors.keySet.contains(entry._1)}
    val debtByDebtOwners: Map[Participant, BigDecimal] = spendingLessCost.filterNot(byNegativeAmounts)

//    println("debt by debtors:  " + debtByDebtors.mkString(", "))
//    println("debt by debt owners:  " + debtByDebtOwners.mkString(", "))

    // TODO try recursively stepping through both maps and making accumulating payments as we go along.

    ExpenseGroup.matchDebtorsToDebtOwners(debtByDebtors, debtByDebtOwners)
  }

  // TODO return a Set instead of a Seq
  // TODO recursively find matching
  // TODO recursively find the rest
  // TODO eliminate 0 values since they are neither owed nor do they owe?
  private def byNegativeAmounts(participantAndAmount: (Participant, BigDecimal)) = participantAndAmount._2 < 0
}

object ExpenseGroup {

  val ZERO = BigDecimal(0)

  def matchDebtorsToDebtOwners(debtors: Map[Participant, BigDecimal], debtOwners: Map[Participant, BigDecimal]): Seq[Payment] = {

    def accPay(payments: Seq[Payment], debtors: Map[Participant, BigDecimal], debtOwners: Map[Participant, BigDecimal]): Seq[Payment] = {

//      if (debtors.isEmpty && debtOwners.isEmpty) {
      if (debtors.isEmpty || debtOwners.isEmpty) {
//      if (debtors.isEmpty) { // } || debtOwners.isEmpty) {
//        assert(debtOwners.isEmpty, s"debt owners remain:  ${debtOwners.mkString(",")}")
        if (!debtOwners.isEmpty) {
          println(s"debt owners remain:  ${debtOwners.mkString(",")}")
        }

        payments
      } else {
        var debt: (Participant, BigDecimal) = debtors.head
        var debtOwnersToUpdate = Map[Participant, BigDecimal]()
        var addPaymentsHere = payments

        for (ownedDebt: (Participant, BigDecimal) <- debtOwners) {
          if (debt._2.equals(ownedDebt._2)) {
//            println("found to be equal:  " + debt + ", " + ownedDebt)

            addPaymentsHere = addPaymentsHere :+ Payment(debt._1, ownedDebt._2, ownedDebt._1)
            debt = (debt._1, 0)
            debtOwnersToUpdate = debtOwners - ownedDebt._1
          } else if (debt._2 > ownedDebt._2) {
            addPaymentsHere = addPaymentsHere :+ Payment(debt._1, ownedDebt._2, ownedDebt._1)
            debt = (debt._1, debt._2 - ownedDebt._2)
            debtOwnersToUpdate = debtOwners - ownedDebt._1 // they're paid off
          } else { // debt._2 < ownedDebt._2
            val payment = Payment(debt._1, debt._2, ownedDebt._1)
            addPaymentsHere = addPaymentsHere :+ payment
            debt = (debt._1, 0) // the debtor has paid their debt

            val remainderOfOwnedDebt = ownedDebt._2 - debt._2
            assert(remainderOfOwnedDebt > 0)
            debtOwnersToUpdate = (debtOwners - ownedDebt._1) + (ownedDebt._1 -> remainderOfOwnedDebt)
          }
        }

        val debtsPostPayments = if (debt._2 == 0) {
          debtors - debt._1 // a debtor no more!
        } else {
          debtors - debt._1 + (debt._1 -> debt._2)
        }

        accPay(addPaymentsHere, debtsPostPayments, debtOwnersToUpdate)
      }
    }

    // TODO this may be too complicated. a simple recursive operation like countChange might be adequate
    // https://bitbucket.org/btgrant/coursera-functional-programming-principles-in-scala/src/019d15eba3706e326a05ba0d81502baab8c11fdd/recfun/src/main/scala/recfun/Main.scala?at=master
    def accumulatePayments(payments: Seq[Payment], debtors: Map[Participant, BigDecimal], debtOwners: Map[Participant, BigDecimal]): Seq[Payment] = {

      println(s"accumulating payments from payment ${payments.mkString(",")}\n\tdebtors ${debtors.mkString(",")}\n\tdebt owners ${debtOwners.mkString(",")}\n\n")

      if (debtors.isEmpty && debtOwners.isEmpty) {
        println("returning payments:  " + payments.mkString(", "))
        payments
      } else {
//        debtors.foldLeft((payments, debtOwners)) {(stuffs) =>
//
//          null
//
//        }


        // TODO do these need to be mutable collections at all? var seems sufficient
        var addPaymentsHere = collection.mutable.Seq[Payment]()
        var mutableDebtors = collection.mutable.Map() ++ debtors
        var mutableDebtOwners = collection.mutable.Map() ++ debtOwners

        // TODO try iterating over the collections passed in but build up modified versions of the collections as we go along?
        for (debt: (Participant, BigDecimal) <- mutableDebtors) {
          for (ownedDebt: (Participant, BigDecimal) <- mutableDebtOwners) {
            if (debt._2.equals(ownedDebt._2)) {
              println("found to be equal:  " + debt + ", " + ownedDebt)

              addPaymentsHere = addPaymentsHere :+ Payment(debt._1, ownedDebt._2, ownedDebt._1)
              mutableDebtors = mutableDebtors - debt._1
              mutableDebtOwners = mutableDebtOwners - ownedDebt._1
            } else if (debt._2 > ownedDebt._2) {
              addPaymentsHere = addPaymentsHere :+ Payment(debt._1, ownedDebt._2, ownedDebt._1)
              mutableDebtors = mutableDebtors - debt._1 + (debt._1 -> (debt._2 - ownedDebt._2))
              mutableDebtOwners = mutableDebtOwners - ownedDebt._1
            } else { // debt._2 is < ownedDebt._2
              val payment = Payment(debt._1, debt._2 , ownedDebt._1)
              assert(payment.amount >= 0)

              addPaymentsHere = addPaymentsHere :+ payment
              mutableDebtors = mutableDebtors - debt._1
              mutableDebtOwners = mutableDebtOwners - ownedDebt._1 + (ownedDebt._1 -> (ownedDebt._2 - payment.amount))
            }
          }
        }

        if (mutableDebtors.isEmpty && mutableDebtOwners.isEmpty) {
          addPaymentsHere
        } else {
          accumulatePayments(addPaymentsHere, mutableDebtors.toMap, mutableDebtOwners.toMap)
        }



//        println("else")
//        debtors.flatMap { debt: (Participant, BigDecimal) =>
////          println("flat mapping debtors")
//          debtOwners.flatMap { ownedDebt: (Participant, BigDecimal) =>
////            println("flat mapping debt owners")
//            if (debt._2.equals(ownedDebt._2)) {
//              //              println("found to be equal:  " + debt + ", " + ownedDebt)
//              accumulatePayments(payments :+ Payment(debt._1, ownedDebt._2, ownedDebt._1), debtors - debt._1, debtOwners - ownedDebt._1)
////              payments ++ matchDebtorsToDebtOwners(debtors - debt._1, debtOwners - ownedDebt._1) :+ Payment(debt._1, ownedDebt._2, ownedDebt._1)
//            } else if (debt._2 > ownedDebt._2) {
//              val payment = Payment(debt._1, debt._2 - ownedDebt._2, ownedDebt._1)
//              val oneLessDebtOwner = debtOwners - ownedDebt._1
//              val debtorsWithLessDebt = debtors - debt._1 + (debt._1 -> (debt._2 - ownedDebt._2))
//
//              accumulatePayments(payments :+ payment, debtorsWithLessDebt, oneLessDebtOwner)
////              payments ++ matchDebtorsToDebtOwners(debtorsWithLessDebt, oneLessDebtOwner) :+ payment
//            } else { // debt._2 is < ownedDebt._2
//              val payment = Payment(debt._1, debt._2 , ownedDebt._1)
//              assert(payment.amount >= 0)
//              val oneLessDebtor = debtors - debt._1
//              val debtOwnersWithLessDebt = debtOwners - ownedDebt._1 + (ownedDebt._1 -> (ownedDebt._2 - payment.amount))
//
//              accumulatePayments(payments :+ payment, oneLessDebtor, debtOwnersWithLessDebt)
////              payments ++ matchDebtorsToDebtOwners(oneLessDebtor, debtOwnersWithLessDebt) :+ payment
//            }
//          }
//        }.toSeq
      }
    }

//    accumulatePayments(Seq(), debtors, debtOwners)/*.toSet.*/toSeq
    accPay(Seq(), debtors, debtOwners)/*.toSet.*/toSeq
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
    expenses.map(_.amount).reduce((a: BigDecimal, b: BigDecimal) => a + b)
  }

}
