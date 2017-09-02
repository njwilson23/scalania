package pl.japila.scalania.s99

object S99_P13 {
  /**
   * Implement the so-called run-length encoding data compression method directly, i.e.
   * don't use other methods you've written (like P09's pack); do all the work directly.
   *
   * scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
   * res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   */

  def _encode_acc[T](acc: (Int, T), ts: Seq[T]): Seq[(Int, T)] =
    ts match {
      case Nil => Seq(acc)
      case a :: b => if (acc._2 == a) _encode_acc(Tuple2(acc._1 + 1, a), b)
      else Seq(acc) ++ _encode_acc(Tuple2(1, a), b)
    }

  def encodeDirect[T](ts: Seq[T]): Seq[(Int, T)] =
    ts match {
      case Nil => Nil
      case a :: b => _encode_acc(Tuple2(1, a), b)
    }

}
