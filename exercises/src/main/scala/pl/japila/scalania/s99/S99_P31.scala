package pl.japila.scalania.s99

object S99_P31 {
  implicit class IntWithIsPrime(n: Int) {
    def isPrime: Boolean = (2 to n - 1).map(n % _ != 0).reduce(_ & _)
  }
}
