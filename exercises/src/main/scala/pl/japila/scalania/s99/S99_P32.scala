package pl.japila.scalania.s99

import scala.math.min

object S99_P32 {
  def gcd(m: Int, n: Int): Int = (1 to min(m, n)).filter((x) => (m % x == 0) & (n % x == 0)).last
}
