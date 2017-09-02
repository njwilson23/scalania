package pl.japila.scalania.s99

object S99_P07 {
  def flatten(ls: Seq[Any]): Seq[Any] =
    ls match {
      case (a: Seq[Any]) :: (b: Seq[Any]) => flatten(a) ++ flatten(b)
      case a :: (b: Seq[Any]) => Seq(a) ++ flatten(b)
      case a :: nil => Seq(a)
      case Nil => Nil
    }
}
