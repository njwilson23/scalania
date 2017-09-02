package pl.japila.scalania.s99

object S99_P02 {

  def penultimate[T](ts: Seq[T]): Option[T] =
    ts.toList match {
      case (a :: (b :: Nil)) => Some(a)
      case a :: tl => penultimate(tl)
      case _ => None
    }

}
