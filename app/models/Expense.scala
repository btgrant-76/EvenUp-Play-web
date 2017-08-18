package models

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

case class Expense(amount: BigDecimal, description: String) {
  require(amount > 0 && description != null && !description.trim.isEmpty)
}

object Expense {
  implicit val jsonReads: Reads[Expense] = (
    (JsPath \ "amount").read[BigDecimal] and
      (JsPath \ "description").read[String]
    )(Expense.apply _)
}
