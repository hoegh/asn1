package asn1.decoding

sealed trait DecodingResult {
  def combineWith(that: DecodingResult): DecodingResult
}

final case class Success() extends DecodingResult {
  override def combineWith(that: DecodingResult): DecodingResult = that
}

final case class Result[T](name: Option[String], value: T) extends DecodingResult {
  override def combineWith(that: DecodingResult): DecodingResult = that match {
    case Failure(_) => that
    case Success() => this
    case res:Result[_] => ResultList(List(this, res))
    case rl:ResultList => ResultList(this :: rl.items)
  }
}

final case class ResultList(items: List[Result[_]]) extends DecodingResult{
  override def combineWith(that: DecodingResult): DecodingResult = that match {
    case Failure(_) => that
    case Success() => this
    case res:Result[_] => ResultList(items :+ res)
    case rl:ResultList => ResultList(items ::: rl.items)
  }
}

final case class Failure(cause: String) extends DecodingResult {
  override def combineWith(that: DecodingResult): DecodingResult = this
}

