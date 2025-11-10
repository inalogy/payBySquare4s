package sk.softwave.paybysquare

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PayBySquareSpec extends AnyFlatSpec with Matchers with PayModelFixtures {

  "PayBySquare" should "properly encode SimplePay to bysquare base-32 string" in {
    val encoded = PayBySquare.encode(simplePay)
    encoded shouldEqual "0007C000F4ACKBVG9BRQ70BD9M6S2QSNEBV38IC4C0526B4AM05848L72VM67KVV3S53O3JU04R23NE0RFGI02B3JLD2HP18NT9U9QG1Q0T3PJN9B01J8BRSV72QJUVIGBOOQBGVB46CEH65UHI46L342P6BCOJ8P40HSGIV4GOUM"
  }

  "PayBySquare" should "properly encode more complex Pay structures to bysquare base-32 string" in {
    val encoded1 = PayBySquare.encode(complexPay1)
    encoded1 shouldEqual "0008O0004EK7RQB80OT9C30S0K5MB7AIN7MIQJTU7DK9NHL1SDJT05V4LNQF1G80ME24II14K4OBODTSP1E61KS1KSBFDRPJ6IA69AVIHEC08D5KUR1C9UQU060TQMABJCEO5RQL8K4S4D2V5M3RG9CTRPPU175HE66UB71RSHFF0VBQCQ73EKQ5O3IHVQPG"

    val encoded2 = PayBySquare.encode(complexPay2)
    encoded2 shouldEqual "0000C0806IID7QOG92SVF5CDAFKL5VRR0L9L8IDMF2T19O6K74A9MNM1F645GNAONFL5QC1T5TFTP3JC0LI7LVEE6UBNE6TAP4Q8T4FCPEJFEIHA787R4BHQQHNO1P5106P9HI3FHIJC90PMCP2B9V4PSE3JVNS3L0CTQFPMC1UKLMHNAKGQ4L7HBHTMIVNT84JNL6PRQKG95DH9OB58LFJQMR4F9R9VU7N2MFDLDPR4AT0605AGEO74T28QSUM1C4MG1BED5BTHR8VI1GOHPK111G"
  }

  "Encode->Decode rountrip" should "result in same serialized string as was encoded" in {
    import PayBySquareParser._

    parse(PayBySquare.encode(simplePay)) shouldEqual simplePay.serialize
    parse(PayBySquare.encode(complexPay1)) shouldEqual complexPay1.serialize
    parse(PayBySquare.encode(complexPay2)) shouldEqual complexPay2.serialize
  }

}

object PayBySquareParser {

  import java.io._
  import java.nio.{ ByteBuffer, ByteOrder }
  import org.tukaani.xz._
  import org.apache.commons.codec.binary.Base32

  private val lc = 3
  private val lp = 0
  private val pb = 2
  private val lzmaProp = (lc + lp * 9 + pb * 9 * 5).toByte
  private val dictSizeKb = 128
  private val dictSizeB = dictSizeKb * 1024

  val base32 = new Base32(true)

  def parse(encoded: String) = {

    val decoded = base32.decode(encoded)
    val (_, lzmaCompressed) = decoded.splitAt(2)
    val (lzmaHeader, lzmaData) = lzmaCompressed.splitAt(2)

    val uncompSize = ByteBuffer.wrap(lzmaHeader).order(ByteOrder.LITTLE_ENDIAN).getShort
    val in = new ByteArrayInputStream(lzmaData)
    val lzmaIn = new LZMAInputStream(in, uncompSize, lzmaProp, dictSizeB)

    try {
      val uncompBytes = Iterator.continually(lzmaIn.read).takeWhile(_ != -1).map(_.toByte).toArray
      //parsing only for testing purposes, so we ignore checksum as we used parsed data for direct comparison with the original input
      val (_, data) = uncompBytes.splitAt(4)
      new String(data, "UTF-8")
    } finally {
      lzmaIn.close()
      in.close()
    }
  }
}
