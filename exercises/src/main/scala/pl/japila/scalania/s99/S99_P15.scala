package pl.japila.scalania.s99

object S99_P15 {
  def rep[T](n: Int, v: T): Seq[T] =
    if (n == 0) Nil
    else Seq(v) ++ rep(n - 1, v)

  def duplicateN[T](n: Int, ts: Seq[T]): Seq[T] =
    ts match {
      case Nil => Nil
      case a :: b => rep(n, a) ++ duplicateN(n, b)
    }
}
