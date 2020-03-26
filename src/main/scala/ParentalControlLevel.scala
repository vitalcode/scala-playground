trait ParentalControlLevel

class Succ[P <: ParentalControlLevel] extends ParentalControlLevel

class <[A <: ParentalControlLevel, B <: ParentalControlLevel]

object ParentalControlLevel {
  class PclU extends ParentalControlLevel
  type pclPG = Succ[PclU]
  type pcl12 = Succ[pclPG]
  type pcl15 = Succ[pcl12]
  type pcl18 = Succ[pcl15]

  val PC_U = new PclU
  val PC_PG = new pclPG
  val PC_12 = new pcl12
  val PC_15 = new pcl15
  val PC_18 = new pcl18

  def apply(pcl: String) = pcl match {
    case "U" => PC_U
    case "PG" => PC_PG
    case "12" => PC_12
    case "15" => PC_15
    case "18" => PC_18
    case _ => throw new IllegalArgumentException()
  }

  def compare[A <: ParentalControlLevel, B <: ParentalControlLevel](a: A, b: B)(implicit evidence: Option[A < B]) = evidence match {
    case Some(_) => -1
    case None if a == b => 0
    case None => 1
  }

  implicit def base[P <: ParentalControlLevel] = Some(new <[PclU, Succ[P]])

  implicit def derived[A <: ParentalControlLevel, B <: ParentalControlLevel](implicit evidence: Some[A < B]) = Some(new <[Succ[A], Succ[B]])
}

