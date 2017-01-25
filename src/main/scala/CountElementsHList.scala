import shapeless._


object CountElementsHList extends App {

  case class Size[L <: HList](get: Int)

  object Size {

    implicit val hnilSize = Size[HNil](0)

    implicit def hconsSize[H, T <: HList](implicit tailSize: Size[T]) = Size[H :: T](1 + tailSize.get)

    def apply[L <: HList](l: L)(implicit size: Size[L]): Int = size.get
  }

  val list = 1 :: 2L :: "other" :: Boolean :: HNil

  println(Size(list))
}


object TypeMatchingHList extends App {

  val list = 1 :: 2L :: "other" :: Boolean :: HNil

  list match {
    case n0 :: n1 :: n2 :: _ => println(s"n0=$n0, n1=$n1, n2=$n2")
    case _ => println("no")
  }
}



