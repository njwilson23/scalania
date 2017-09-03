package pl.japila.scalania.s99

object S99_P16 {
  def _dropin[T](n: Int, rem: Int, ts: Seq[T]): Seq[T] =
    if (ts.isEmpty) Nil
    else if (rem == 0) _dropin(n, n - 1, ts.tail)
    else Seq(ts.head) ++ _dropin(n, rem - 1, ts.tail)

  def drop[T](n: Int, ts: Seq[T]): Seq[T] = _dropin(n, n - 1, ts)
}
