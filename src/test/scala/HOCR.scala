import org.scalatest.{FreeSpec, Matchers}

import scala.io.Source
import scala.math.BigDecimal
import scala.util.Try
import scala.xml.{NodeSeq, XML}

case class Result(items: Seq[(String, String)], total: String) {
  override def toString = (items :+ ("TOTAL" -> total)  map {
    case (k, v) => s"$k -> $v"
  }).mkString("\n")
}

class HOCR extends FreeSpec with Matchers {

  trait OrcNode[T] {
    def apply(nodes: NodeSeq): Seq[T]
  }

  implicit class NodeSeqExtended(nodes: NodeSeq) {
    def withAttribute(keyValue: (String, String)) = nodes.filter(n => n.attributes.exists(a => a.key == keyValue._1 && a.value.text == keyValue._2))

    def getOrcNodes[T](implicit orcNode: OrcNode[T]): Seq[T] = {
      orcNode(nodes)
    }
  }

  case class OcrWord(s: String) {
    def cost: Option[BigDecimal] = {
      val numString = s.replaceAll("\\p{Sc}|\\p{L}|,", "")
      if (s.contains('£')) {
        Try(BigDecimal(numString)).toOption
      }
      else None
    }

    override def toString = s
  }

  case class OcrLine(words: Seq[OcrWord]) {
    def itemCost: Option[(String, String)] = {
      val values = words.collect {
        case v if v.cost.isDefined => s"${v.cost.get}"
      }
      val keys = words.filter(w => !values.exists(v => w.s.contains(v)))

      if (values.nonEmpty) {
        Some((keys.mkString(" "), values.mkString(",")))
      } else None
    }

    override def toString = itemCost match {
      case Some((k, v)) => s"$k -> $v"
      case None => words.mkString(" ")
    }
  }

  case class OcrArea(lines: Seq[OcrLine]) {
    override def toString = s"${lines.mkString("\n")}"
  }

  implicit val ocrWord = new OrcNode[OcrWord] {
    def apply(nodes: NodeSeq): Seq[OcrWord] = (nodes \\ "_" withAttribute ("class" -> "ocrx_word")).map(n => OcrWord(n.text.replace(" ", "")))
  }

  implicit val ocrLine = new OrcNode[OcrLine] {
    def apply(nodes: NodeSeq): Seq[OcrLine] = (nodes \\ "_" withAttribute ("class" -> "ocr_line")).map(n => OcrLine(n.getOrcNodes[OcrWord]))
  }

  implicit val ocrArea = new OrcNode[OcrArea] {
    def apply(nodes: NodeSeq): Seq[OcrArea] = (nodes \\ "_" withAttribute ("class" -> "ocr_carea")).map(n => OcrArea(n.getOrcNodes[OcrLine]))
  }

  "Parse hocr file" in {
    val file = Source.fromResource("hocr.xml").mkString
    val fileXml = file.splitAt(file.indexOf("<html"))._2.filter(_ >= ' ')
    val xml = XML.loadString(fileXml)

    val ocrAreasXml = xml \\ "_" withAttribute ("class" -> "ocr_carea")

    val ocrAreas = xml.getOrcNodes[OcrArea]

    println(ocrAreas.mkString("\n\n\n"))


    val items = for {
      a <- ocrAreas
      l <- a.lines
      items <- l.itemCost
    } yield items

    val total = items.filter(_._1.toLowerCase.contains("balance")).head

    val result = Result(
      items = items.filter(!_._1.contains(total._1)),
      total = total._2
    )

    println(result)

    ocrAreasXml.length shouldBe 6
  }
}
