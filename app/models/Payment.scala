package models

case class Payment(from: Participant, amount: BigDecimal,  to: Participant)
