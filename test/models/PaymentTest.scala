package models

import org.scalatest.FunSuite

object PaymentTest {
  val from = Participant("participant 1")
  val to = Participant("participant 2")
}

class PaymentTest extends FunSuite {
  import PaymentTest._

  test("A payment that doesn't need to be rounded is equal to the original payment") {
    val payment = Payment(from, 25.08, to)
    assert(Payment.toRoundedPayment(payment).amount === 25.08)
    assert(payment.rounded.amount === 25.08)
  }

  test("A payment that will be rounded up") {
    val payment = Payment(from, 25.125, to)
    assert(Payment.toRoundedPayment(payment).amount === 25.13)
    assert(payment.rounded.amount === 25.13)
  }

  test("A payment that will be rounded down") {
    val payment = Payment(from, 25.122, to)
    assert(Payment.toRoundedPayment(payment).amount === 25.12)
    assert(payment.rounded.amount === 25.12)
  }

}
