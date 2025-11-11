package sk.softwave.paybysquare

import java.time.LocalDate
import java.time.format.DateTimeFormatter

sealed trait BySquareType {
  def serialize: String
}

case class Payment(
  payType: Int,
  amount: BigDecimal,
  currency: String,
  dueDate: Option[LocalDate],
  vs: Option[String],
  ss: Option[String],
  ks: Option[String],
  reference: Option[String],
  paymentNote: Option[String],
  bankAccounts: Seq[BankAccount],
  beneficiaryName: Option[String] = None,
  beneficiaryStreet: Option[String] = None,
  beneficiaryCity: Option[String] = None
) extends BySquareType {

  require(payType == 1, "StandingOrder and DirectDebit unsupported for now")

  override def serialize: String =
    Seq(
      payType,
      amount.setScale(2),
      currency,
      dueDate.map(_.format(DateTimeFormatter.ofPattern("yyyyMMdd"))).getOrElse(""),
      vs.getOrElse(""),
      ks.getOrElse(""),
      ss.getOrElse(""),
      reference.getOrElse("").replace("\t", " "),
      paymentNote.getOrElse("").replace("\t", " "),
      bankAccounts.size,
      bankAccounts.map(_.serialize).mkString("\t"),
      0, // simplification as StandingOrder is unsupported for now
      0 // simplification as DirectDebit is unsupported for now
    ).mkString("\t")
}

case class BankAccount(iban: String, bic: Option[String]) extends BySquareType {
  override def serialize: String = Seq(iban, bic.getOrElse("")).mkString("\t")
}

trait Pay extends BySquareType {
  def invoiceId: Option[String]
  def payments: Seq[Payment]

  //TODO remove
  def serializeTest: String = serialize.replace("\t", "|")

  override def serialize: String = {
    val paymentsData = Seq(
      invoiceId.getOrElse(""),
      payments.size,
      payments.map(_.serialize).mkString("\t")
    ).mkString("\t")

    // Append beneficiary info for each payment (PayBySquare 1.1.0+)
    // Always append 3 fields per payment: name, street, city
    val beneficiaryData = payments.map { p =>
      Seq(
        p.beneficiaryName.getOrElse(""),
        p.beneficiaryStreet.getOrElse(""),
        p.beneficiaryCity.getOrElse("")
      ).mkString("\t")
    }.mkString("\t")

    paymentsData + "\t" + beneficiaryData
  }
}

case class SimplePay(
  amount: BigDecimal,
  currency: String,
  vs: Option[String],
  ss: Option[String],
  ks: Option[String],
  reference: Option[String],
  paymentNote: Option[String],
  iban: String,
  bic: Option[String],
  beneficiaryName: Option[String] = None,
  beneficiaryStreet: Option[String] = None,
  beneficiaryCity: Option[String] = None
) extends Pay {

  override val invoiceId = None

  override val payments = Seq(
    Payment(
      payType = 1,
      amount = amount,
      currency = currency,
      dueDate = None,
      vs = vs,
      ss = ss,
      ks = ks,
      reference = reference,
      paymentNote = paymentNote,
      bankAccounts = Seq(BankAccount(iban, bic)),
      beneficiaryName = beneficiaryName,
      beneficiaryStreet = beneficiaryStreet,
      beneficiaryCity = beneficiaryCity
    )
  )
}
