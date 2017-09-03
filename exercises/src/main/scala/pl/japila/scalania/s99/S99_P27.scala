package pl.japila.scalania.s99
import scala.math.Ordering.Implicits._

object S99_P27 {
  type GroupFn[T] = Seq[T] => Seq[Seq[Seq[T]]]
  type GroupGeneralizedFn[T] = (Seq[Int], Seq[T]) => Seq[Seq[Seq[T]]]

  def solutions[T]: List[(String, GroupFn[T])] = List(
    ("my own implementation", group)
  )

  def solutionsGen[T](): List[(String, GroupGeneralizedFn[T])] = List(
    ("my own implementation", groupGeneralized)
  )

  def group[T](ts: Seq[T]): Seq[Seq[Seq[T]]] = groupGeneralized(Seq(2, 3, 4), ts)

  def prependToFirst[T](thing: Seq[Seq[T]], item: T) = Seq(Seq(item) ++ thing.head, thing.last)
  def prependToSecond[T](thing: Seq[Seq[T]], item: T) = Seq(thing.head, Seq(item) ++ thing.last)

  // return all partitions of two groups: one of n items, and one of N-n items
  def partition[T](n: Int, ts: Seq[T]): Seq[Seq[Seq[T]]] = {
    if (n == ts.length) Seq(Seq(ts, Nil))
    else if (n == 1) partition(n, ts.drop(1)).map(prependToSecond(_, ts.head)) ++ Seq(Seq(ts.take(1), ts.drop(1)))
    else {
      partition(n, ts.drop(1)).map(prependToSecond(_, ts.head)) ++
        partition(n - 1, ts.drop(1)).map(prependToFirst(_, ts.head))
    }
  }

  def groupGeneralized[T](groups: Seq[Int], ts: Seq[T]): Seq[Seq[Seq[T]]] =
    groups match {
      case a :: Nil => partition(a, ts).map((x: Seq[Seq[T]]) => x.take(1))
      case a :: b => {
        // select the first group
        val parts = partition(a, ts)
        // compute the next groups
        val remainder = parts.map((x: Seq[Seq[T]]) => groupGeneralized(b, x.last))
        // merge by prepending
        parts.map(_.take(1)).zip(remainder).flatMap((pr) =>
          pr._2.map(pr._1 ++ _))
      }
    }

}
