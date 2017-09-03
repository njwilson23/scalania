package pl.japila.scalania.s99

object S99_P26 {
  type CombinationsFn[T] = (Int, Seq[T]) => Seq[Seq[T]]

  def solutions[T](): List[(String, CombinationsFn[T])] = List(
    ("my own implementation", combinations[T])
  )

  def combinations[T](count: Int, ts: Seq[T]): Seq[Seq[T]] = {
    if (count == 0) Nil
    else if (count == ts.length) Seq(ts)
    else if (count == 1) ts.map((x: T) => Seq(x))
    else combinations(count - 1, ts.drop(1)).map((x: Seq[T]) => { ts.take(1) ++ x }) ++ combinations(count, ts.drop(1))
  }

}
