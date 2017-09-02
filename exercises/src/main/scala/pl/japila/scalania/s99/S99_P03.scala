package pl.japila.scalania.s99

object S99_P03 {

  def nth[T](n: Int, ts: Seq[T]): Option[T] =
    if (ts.isEmpty) None
    else if (n == 0) Some(ts.head)
    else nth(n - 1, ts.tail)

}
