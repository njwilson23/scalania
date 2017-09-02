package pl.japila.scalania.s99

object S99_P10 {

  def _encode_acc[T](acc: (Int, T), ts: Seq[T]): Seq[(Int, T)] =
    ts match {
      case Nil => Seq(acc)
      case a :: b => if (acc._2 == a) _encode_acc(Tuple2(acc._1 + 1, a), b)
      else Seq(acc) ++ _encode_acc(Tuple2(1, a), b)
    }

  def encode[T](ts: Seq[T]): Seq[(Int, T)] =
    ts match {
      case Nil => Nil
      case a :: b => _encode_acc(Tuple2(1, a), b)
    }

}
