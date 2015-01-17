package models

class Expense(val amount: BigDecimal, val description: String) {
  require(amount > 0 && !description.isEmpty)
}
