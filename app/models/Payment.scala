package models

import scala.math.BigDecimal.RoundingMode

case class Payment(from: Participant, amount: BigDecimal, to: Participant) {
  assume(from != null && to != null && amount > 0)

  def rounded =
    Payment(this.from, this.amount.setScale(2, RoundingMode.HALF_UP), this.to)

}

object Payment {
  def toRoundedPayment(payment: Payment) =
    Payment(payment.from, payment.amount.setScale(2, RoundingMode.HALF_UP), payment.to)
}
