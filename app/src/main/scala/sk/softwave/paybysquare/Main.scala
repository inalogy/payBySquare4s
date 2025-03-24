package sk.softwave.paybysquare

import scala.util.{ Failure, Success, Try }

object Main extends App {
  val toEncode = if (args.length > 0) args(0) else ""

  val usageStr = "Usage: paybysquare <payload>"
  val payloadUsageStr = "payload - <amount>;<currency>;<vs>;<ss>;<ks>;<reference>;<paymentNote>;<iban>;<bic>"

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
    if (data.size != 9) {
      System.err.println(payloadUsageStr)
      System.err.println(s"Expected 9 parameters, got ${data.size}")
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
          bic = strToOpt(data(8))
        )
        
        // Generate QR code and encode it to Base64
        val base64Image = PayBySquare.encodeFrameQRToBase64(pay)
        
        // Output Base64 string to stdout
        print(base64Image)
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
