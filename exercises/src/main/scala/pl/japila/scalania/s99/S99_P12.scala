package pl.japila.scalania.s99

object S99_P12 {

  def rep[T](n: Int, v: T): Seq[T] =
    if (n == 0) Nil
    else Seq(v) ++ rep(n - 1, v)

  def decode[T](its: Seq[(Int, T)]): Seq[T] =
    its match {
      case Nil => Nil
      case a :: b => rep(a._1, a._2) ++ decode(b)
    }

}
