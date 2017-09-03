package pl.japila.scalania.s99

import scala.util.Random

object S99_P24 {
  type LottoFn = (Int, Int) => Seq[Int]

  val solutions: List[(String, LottoFn)] = List(
    ("my own implementation", lotto)
  )

  def lotto(count: Int, max: Int): Seq[Int] = {
    val rand = new Random()
    rand.shuffle(1 to max).take(count)
  }
}
