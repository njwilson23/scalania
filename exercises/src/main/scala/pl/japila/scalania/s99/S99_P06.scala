package pl.japila.scalania.s99

object S99_P06 {

  def isPalindrome[T](ts: Seq[T]): Boolean =
    ts match {
      case Nil => false
      case a :: Nil => true
      case a :: b => if (a == b.last) isPalindrome(b.take(b.length - 1))
      else false
    }

}
