import org.scalatest.{FreeSpec, Matchers}
import shapeless.{:+:, CNil, Coproduct, Generic, Inl, Inr, _}
import ops._

class TypeResurrection extends FreeSpec with Matchers {

  sealed trait Puple

  case class Boy(name: String) extends Puple

  case class Girl(name: String) extends Puple

  trait Grade[T] {
    def giveGrade(t: T): String
  }

  def apply[T](t: T)(implicit grade: Grade[T]) = grade

  def createGrade[T](func: T => String): Grade[T] =
    new Grade[T] {
      def giveGrade(t: T): String = func(t)
    }

  implicit val gradeForBoy = new Grade[Boy] {
    override def giveGrade(t: Boy) = "A"
  }

  implicit val gradeForGirl = new Grade[Girl] {
    override def giveGrade(t: Girl) = "C"
  }

  "Should be able to ..." in {

    val gen = Generic[Puple]

    implicit val cnilEncoder: Grade[CNil] = new Grade[CNil] {
      override def giveGrade(t: CNil) = throw new Exception("Inconceivable!")
    }

    implicit def coproductEncoder[H, T <: Coproduct](implicit
                                                     hGrade: Grade[H],
                                                     tGrade: Grade[T]
                                                    ): Grade[H :+: T] = createGrade {
      case Inl(h) => hGrade.giveGrade(h)
      case Inr(t) => tGrade.giveGrade(t)
    }

    def apply[T](t: T)(implicit grade: Grade[T]) = grade

    def test[H](puple: H)(implicit grade: Grade[H]): String = {
      grade.giveGrade(puple)
    }

    def testCo[H <: Coproduct](puple: H)(implicit grade: Grade[H]): String = {
      grade.giveGrade(puple)
    }

    val andrey = Boy("Andrey").asInstanceOf[Puple]
    val anna = Girl("Anna").asInstanceOf[Puple]

    val b = gen.to(andrey) match {
      case Inl(t) => t
    }
    val g = gen.to(anna) match {
      case Inr(Inl(t)) => t
    }

//    implicit def genericTest[A, B, R <: Coproduct](pp: A)(implicit gen: Generic.Aux[A, R]) =
//      gen.to(pp).tail
//
//    val gg = genericTest(anna)


//    def instance[C <: Coproduct, H](co: C): H  = co match {
//      case Inl(t) => test(t).asInstanceOf[H]
//      case Inr(t: Coproduct) => instance(t)
//    }

    //instance(gen.to(anna)) shouldBe "A"

    //testCo(gen.to(andrey)) shouldBe "A"

    test(b) shouldBe "A"
    test(g) shouldBe "C"

  }
}

/*
/** JSON ADT */
sealed abstract class FunJson

final case class FunJsonObject(fields: List[(String, FunJson)]) extends FunJson

final case class FunJsonArray(items: List[FunJson]) extends FunJson

final case class FunJsonString(value: String) extends FunJson

final case class FunJsonNumber(value: Double) extends FunJson

final case class FunJsonBoolean(value: Boolean) extends FunJson

case object FunJsonNull$ extends FunJson


/** Stringification methods */
object FunJson {
  def stringify(json: FunJson): String = json match {
    case FunJsonObject(fields) => "{" + fields.map(stringifyField).mkString(",") + "}"
    case FunJsonArray(items) => "[" + items.map(stringify).mkString(",") + "]"
    case FunJsonString(value) => "\"" + escape(value) + "\""
    case FunJsonNumber(value) => value.toString
    case FunJsonBoolean(value) => value.toString
    case FunJsonNull$ => "null"
  }

  private def stringifyField(field: (String, FunJson)): String = {
    val (name, value) = field
    escape(name) + ":" + stringify(value)
  }

  private def escape(str: String): String =
    "\"" + str.replaceAll("\"", "\\\\\"") + "\""
}


/**
  * Type class for encoding a value of type A as JSON.
  */
trait JsonEncoder[A] {
  def encode(value: A): FunJson
}

/**
  * Specialization of JsonEncoder
  * for encoding as a JSON object.
  *
  * We introduce this because
  * the encoder for :: has to asemble an object
  * from fields form the head and tail.
  * Having JsonObjectEncoder avoids annoying
  * pattern matching to ensure the encoded tail
  * is indeed an object.
  */
trait JsonObjectEncoder[A] extends JsonEncoder[A] {
  def encode(value: A): FunJsonObject
}


object JsonEncoder {
  /** Helper: create a JsonEncoder from a plain function */
  def pure[A](func: A => FunJson): JsonEncoder[A] =
  new JsonEncoder[A] {
    def encode(value: A): FunJson =
      func(value)
  }

  /** Helper: create a JsonObjectEncoder from a plain function */
  def pureObj[A](func: A => FunJsonObject): JsonObjectEncoder[A] =
  new JsonObjectEncoder[A] {
    def encode(value: A): FunJsonObject =
      func(value)
  }

  // JsonEncoder instances for primitive types:

  implicit val stringEncoder: JsonEncoder[String] =
    pure(str => FunJsonString(str))

  implicit val intEncoder: JsonEncoder[Int] =
    pure(num => FunJsonNumber(num))

  implicit val doubleEncoder: JsonEncoder[Double] =
    pure(num => FunJsonNumber(num))

  implicit val booleanEncoder: JsonEncoder[Boolean] =
    pure(bool => FunJsonBoolean(bool))

  implicit def optionEncoder[A](implicit encoder: JsonEncoder[A]): JsonEncoder[Option[A]] =
    pure(opt => opt.map(encoder.encode).getOrElse(FunJsonNull$))

  implicit def listEncoder[A](implicit encoder: JsonEncoder[A]): JsonEncoder[List[A]] =
    pure(list => FunJsonArray(list.map(encoder.encode)))

  // JsonEncoder instances for HLists.
  //
  // Notice that hlistEncoder produces an instance for:
  //
  //     JsonObjectEncoder[FieldType[K, H] :: T]
  //
  // FieldType[K, H] represents a type H tagged with K.
  // LabelledGeneric tags its component types
  // with the Symbolic literal types of the field names,
  // so we can use a Witness.Aux[K] to retrieve
  // the field name as a value.

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    pureObj(hnil => FunJsonObject(Nil))

  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
                                                         implicit
                                                         witness: Witness.Aux[K],
                                                         hEncoder: Lazy[JsonEncoder[H]],
                                                         tEncoder: JsonObjectEncoder[T]
                                                       ): JsonObjectEncoder[FieldType[K, H] :: T] =
    pureObj {
      case h :: t =>
        val hField = witness.value.name -> hEncoder.value.encode(h)
        val tFields = tEncoder.encode(t).fields
        FunJsonObject(hField :: tFields)
    }

  // JsonEncoder instances for Coproducts:
  //
  // The notes above for hlistEncoder also apply to
  // coproductEncoder, except that
  // K represents a type name, not a field name.

  implicit val cnilEncoder: JsonObjectEncoder[CNil] =
    pureObj(cnil => ???)

  implicit def coproductEncoder[K <: Symbol, H, T <: Coproduct](
                                                                 implicit
                                                                 witness: Witness.Aux[K],
                                                                 hEncoder: Lazy[JsonEncoder[H]],
                                                                 tEncoder: JsonObjectEncoder[T]
                                                               ): JsonObjectEncoder[FieldType[K, H] :+: T] =
    pureObj {
      case Inl(h) => FunJsonObject(List(witness.value.name -> hEncoder.value.encode(h)))
      case Inr(t) => tEncoder.encode(t)
    }

  // JsonEncoder instance for LabelledGeneric:

  implicit def genericEncoder[A, R](
                                     implicit
                                     gen: LabelledGeneric.Aux[A, R],
                                     enc: Lazy[JsonEncoder[R]]
                                   ): JsonEncoder[A] =
    pure(a => enc.value.encode(gen.to(a)))
}

object Main extends App {
  // Entry point for JsonEncoder:

  def encodeJson[A](value: A)(implicit encoder: JsonEncoder[A]): FunJson =
    encoder.encode(value)

  sealed trait Shape

  final case class Rectangle(width: Double, height: Double) extends Shape

  final case class Circle(radius: Double) extends Shape

  val shapes: List[Shape] =
    List(
      Rectangle(1, 2),
      Circle(3),
      Rectangle(4, 5),
      Circle(6)
    )

  val optShapes: List[Option[Shape]] =
    List(
      Some(Rectangle(1, 2)),
      Some(Circle(3)),
      None,
      Some(Rectangle(4, 5)),
      Some(Circle(6)),
      None
    )

  println("Shapes " + shapes)
  println("Shapes as AST: " + encodeJson(shapes))
  println("Shapes as JSON: " + FunJson.stringify(encodeJson(shapes)))
  println("Optional shapes " + optShapes)
  println("Optional shapes as AST: " + encodeJson(optShapes))
  println("Optional shapes as JSON: " + FunJson.stringify(encodeJson(optShapes)))
}
*/