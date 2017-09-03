package pl.japila.scalania.s99

object S99_P19 {

  def rotate[T](n: Int, ts: Seq[T]): Seq[T] = {
    val dist = n % ts.length
    if (dist >= 0) ts.drop(dist) ++ ts.take(dist)
    else ts.drop(ts.length + dist) ++ ts.take(ts.length + dist)
  }

}
