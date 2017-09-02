package pl.japila.scalania.s99

object S99_P08 {
  def compress[T](ts: Seq[T]): Seq[T] =
    ts match {
      case Nil => Nil
      case a :: Nil => Seq(a)
      case a :: (b :: c) => if (a == b) compress(a :: c)
      else Seq(a) ++ compress(b :: c)
    }
}
