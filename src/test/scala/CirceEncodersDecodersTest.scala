import io.circe.{Json, _}
import io.circe.parser._
import io.circe.syntax._
import org.scalatest.{FreeSpec, Matchers}

class CirceEncodersDecodersTest extends FreeSpec with Matchers {

  case class Model(pString: String, pJson: String)

  import shapeless._


  def createLabelledGeneric[A, HIn, HOut](x: LabelledGeneric.Aux[A, HIn])(f: HIn => HOut)(g: HOut => HIn) = new LabelledGeneric[A] {
    override type Repr = HOut

    override def to(t: A): HOut = f(x.to(t))

    override def from(r: HOut): A = x.from(g(r))
  }

  def stringToJson(s: String): Json = {
    parser.parse(s).getOrElse(Json.Null)
  }

  def jsonToString(j: Json): String = {
    j.noSpaces
  }

//  "json modification during encoding/decodering" - {
//    "using semiauto and custom LabelledGeneric for given model" in new Context {
//
//      import io.circe.generic.semiauto._
//
//      implicit val customModelGeneric = createLabelledGeneric(LabelledGeneric[Model])(d => _.updateWith('pJson)(stringToJson))(_.updateWith('pJson)(jsonToString))
//
//      implicit val modelEncode: Encoder[Model] = deriveEncoder
//      implicit val modelDecoder: Decoder[Model] = deriveDecoder
//
//      aModel.asJson shouldEqual aJsonDeep
//      aJsonDeep.as[Model].toOption.get shouldEqual aModel
//    }
//  }

  "semiauto: derive encoder/decoder" - {
    import io.circe.generic.semiauto._
    "case class encode into json" in new Context {
      implicit val modelEncode: Encoder[Model] = deriveEncoder
      aModel.asJson shouldEqual aJsonFlat
    }

    "json decode into case class" in new Context {
      implicit val modelDecoder: Decoder[Model] = deriveDecoder
      aJsonFlat.as[Model].toOption.get shouldEqual aModel
    }
    //        "Hlist into json" in new Context {
    //          import shapeless.LabelledGeneric
    //          val modelGen = LabelledGeneric[Model]
    //          val modelRepr = modelGen.to(aModel)
    //
    //          modelRepr.asJson shouldEqual aJsonFlat
    //        }
  }

  "auto: automatic derivation of encoder/decoder" - {
    "case class" in new Context {

      import io.circe.generic.auto._

      aModel.asJson shouldEqual aJsonFlat
    }
  }

  "custom encoders/decoders" - {
    "case class" in new Context {
      implicit val modelEncoder: Encoder[Model] = new Encoder[Model] {
        override def apply(a: Model): Json = {
          Json.obj(
            "pString" -> Json.fromString(a.pString),
            "pJson" -> Json.fromString(a.pJson)
          )
        }
      }
      aModel.asJson shouldEqual aJsonFlat
    }
  }

  trait Context {
    val aModel = Model(
      pString = "Lisenok",
      pJson = """{"name":"Lisenok","profession":"Accountant"}"""
    )

    val aJsonDeep = parse(
      """
        {
          "pString": "Lisenok",
          "pJson": {
            "name": "Lisenok",
            "profession": "Accountant"
          }
        }
      """).getOrElse(Json.Null)

    val aJsonFlat = parse(
      """
        {
          "pString": "Lisenok",
          "pJson": "{\"name\":\"Lisenok\",\"profession\":\"Accountant\"}"
        }
      """).getOrElse(Json.Null)
  }

}
