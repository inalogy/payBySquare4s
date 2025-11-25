package sk.softwave.paybysquare

import scala.util.{ Failure, Success, Try }

object Main extends App {
  val toEncode = if (args.length > 0) args(0) else ""

  val usageStr = "Usage: paybysquare <payload>"
  val payloadUsageStr = "payload - <amount>;<currency>;<vs>;<ss>;<ks>;<reference>;<paymentNote>;<iban>;<bic>;<beneficiaryName>"

  private def strToOpt(str: String): Option[String] = if (str.isEmpty) None else Some(str)

  if (args.length == 1 && args(0) == "--help") {
    System.err.println(usageStr)
    System.err.println(payloadUsageStr)
    System.err.println("The Base64 encoded PNG image will be written to stdout")
  } else if (toEncode.isEmpty) {
    System.err.println(usageStr)
    sys.exit(1)
  } else {
    val data = toEncode.replace("\\;", 0.toChar.toString).split(";", -1).map(_.replace(0.toChar, ';'))
    if (data.size != 10) {
      System.err.println(payloadUsageStr)
      System.err.println(s"Expected 10 parameters, got ${data.size}")
      sys.exit(2)
    } else {
      Try {
        val pay = SimplePay(
          amount = BigDecimal(data(0)),
          currency = data(1),
          vs = strToOpt(data(2)),
          ss = strToOpt(data(3)),
          ks = strToOpt(data(4)),
          reference = strToOpt(data(5)),
          paymentNote = strToOpt(data(6)),
          iban = data(7),
          bic = strToOpt(data(8)),
          beneficiaryName = strToOpt(data(9))
        )

        val base64Data = PayBySquare.encode(pay)
        System.out.println(base64Data)
      } match {
        case Success(_) =>
        case Failure(e) =>
          System.err.println(e.getMessage)
          System.err.println(payloadUsageStr)
          sys.exit(3)
      }
    }
  }
}
