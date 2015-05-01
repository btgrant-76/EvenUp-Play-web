package models

import scala.math.BigDecimal.RoundingMode

case class Payment(from: Participant, amount: BigDecimal, to: Participant)

object Payment {
  def toRoundedPayment(payment: Payment) =
    Payment(payment.from, payment.amount.setScale(2, RoundingMode.HALF_UP), payment.to)

}
