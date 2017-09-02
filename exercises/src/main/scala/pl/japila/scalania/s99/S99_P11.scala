package pl.japila.scalania.s99

object S99_P11 {

  def _incr[T](acc: Either[(Int, T), T], v: T): (Int, T) =
    acc match {
      case Right(a) => (2, a)
      case Left((cnt, a)) => (cnt + 1, a)
    }

  def _matches[T](acc: Either[(Int, T), T], v: T): Boolean =
    acc match {
      case Right(a) => a == v
      case Left((cnt, a)) => a == v
    }

  def _encode_acc[T](acc: Either[(Int, T), T], ts: Seq[T]): Seq[Either[(Int, T), T]] =
    ts match {
      case Nil => Seq(acc)
      case a :: b => if (_matches(acc, a)) _encode_acc(Left(_incr(acc, a)), b)
      else Seq(acc) ++ _encode_acc(Right(a), b)
    }

  def encodeModified[T](ts: Seq[T]): Seq[Either[(Int, T), T]] =
    ts match {
      case Nil => Nil
      case a :: b => _encode_acc(Right(a), b)
    }

}
