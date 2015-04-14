package models

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

case class Expense(amount: BigDecimal, name: String) {
  require(amount > 0 && !name.isEmpty)
}

object Expense {
  implicit val jsonReads: Reads[Expense] = (
    (JsPath \ "amount").read[BigDecimal] and
      (JsPath \ "name").read[String]
    )(Expense.apply _)
}
