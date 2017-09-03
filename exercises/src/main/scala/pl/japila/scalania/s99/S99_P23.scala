package pl.japila.scalania.s99

import scala.util.Random

object S99_P23 {
  type RandomSelectFn[T] = (Int, Seq[T]) => Seq[T]

  def solutions[T]: List[(String, RandomSelectFn[T])] = List(
    ("my own implementation", randomSelect)
  )

  def randomSelect[T](count: Int, ts: Seq[T]): Seq[T] = {
    val rand = new Random()
    rand.shuffle(ts).take(count)
  }
}
