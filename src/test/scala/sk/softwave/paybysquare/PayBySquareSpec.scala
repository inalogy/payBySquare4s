package sk.softwave.paybysquare

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PayBySquareSpec extends AnyFlatSpec with Matchers with PayModelFixtures {

  "PayBySquare" should "properly encode SimplePay to bysquare base-32 string" in {
    val encoded = PayBySquare.encode(simplePay)
    encoded shouldEqual "0007G000B8H1KGA09319N3GVVVJEIM85EQ2PO147D5VCEB9F1VA47IMFF3PLA1Q9TDPQ4R5NQ38B8CN1GSU0RMERLJN23NE2H6RM3J1UPG2GIBS4GPQIK8MQ63E9IFSML4ET47RCAQ9871A9DSAANDOU82TQ7U9UFN0FBHK9EA1CS00"
  }

  "PayBySquare" should "properly encode more complex Pay structures to bysquare base-32 string" in {
    val encoded1 = PayBySquare.encode(complexPay1)
    encoded1 shouldEqual "0008Q0000M73IPTPAS10487D211QREU6A7MB5QHCL063BNELNG9ETRTD7I2UPSH25HM54CP5802D598IMM8M6MT5NQSIBQSNV7D6IHBOGRPG63563MNVQ5U7N17011N6I03U9186PDUQH6SFARHFF43BG5NGFMM3BFD738UQIF729RNK8HMKIKSON483N000"

    val encoded2 = PayBySquare.encode(complexPay2)
    encoded2 shouldEqual "0000808036PLC93BP31L28R9ASAOHSR9NIUBKCR2VM2SGNVDQRA9KUM6D4RO4NRC2LS6V1K96IKI4U5AUU0838IMUQBK002HUA8RRN03JSEB8NU2VS33GNEDTMPH617LLLNORTMDGKCIK1J15GG3V4OSKO68NK6THJOF5KEPE68HRCDN717UNSEO0AC772LMOUSHMQH8K78KSMOMQ2KEDM9JR4AFS8HMSGJPC3IBHVL4GSR9JSKG4DUP1T3DBLBL02OLS3V172DEJK20PSJSJ6G8TS00"
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
