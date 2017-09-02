package pl.japila.scalania.s99

object S99_P04 {

  def length[T](ts: Seq[T]): Int = acc(0, ts)

  def acc[T](cnt: Int, ts: Seq[T]): Int =
    if (ts.isEmpty) cnt
    else acc(cnt + 1, ts.tail)

}
