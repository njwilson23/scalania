package pl.japila.scalania.s99

object S99_P14 {
  def duplicate[T](ts: Seq[T]): Seq[T] =
    ts match {
      case Nil => Nil
      case a :: b => Seq(a, a) ++ duplicate(b)
    }
}
