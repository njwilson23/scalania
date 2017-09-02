package pl.japila.scalania.s99

import scala.util.{ Success, Failure, Try }

object S99_P01 {

  def last[T](s: Seq[T]): Try[T] =
    s.toList match {
      case a :: Nil => Success(a)
      case a :: tl => last(tl)
      case Nil => Failure(new java.util.NoSuchElementException("empty seq"))
      case _ => Failure(new Exception("something completely unexpected"))
    }

  /*def last[T](s: Seq[T]): Try[T] =
    if (s.isEmpty) Failure(new java.util.NoSuchElementException("empty seq"))
    else if (s.length == 1) Success(s.head)
    else last(s.tail)*/

}
