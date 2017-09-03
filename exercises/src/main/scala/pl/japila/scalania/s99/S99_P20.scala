package pl.japila.scalania.s99

object S99_P20 {
  type RemoveAtFn = (Int, Seq[Any]) => (Seq[Any], Any)

  val solutions: List[(String, RemoveAtFn)] = List(
    ("my own implementation", removeAt)
  )

  def removeAt[T](n: Int, ts: Seq[T]): (Seq[Any], Any) = (ts.take(n) ++ ts.drop(n + 1), ts.take(n + 1).last)

}
