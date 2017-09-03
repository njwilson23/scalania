package pl.japila.scalania.s99

object S99_P28 {
  type LsortFn[T] = Seq[Seq[T]] => Seq[Seq[T]]
  type LsortFreqFn[T] = Seq[Seq[T]] => Seq[Seq[T]]

  def solutions[T]: List[(String, LsortFn[T])] = List(
    ("my own implementation", lsort)
  )

  def solutionsFreq[T]: List[(String, LsortFreqFn[T])] = List(
    ("my own implementation", lsortFreq)
  )

  def lsort[T](ts: Seq[Seq[T]]): Seq[Seq[T]] = ts.sortWith(_.length < _.length)

  def _freq[T](len: Int, tss: Seq[Seq[T]]): Int = tss.filter(_.length == len).length
  def lsortFreq[T](tss: Seq[Seq[T]]): Seq[Seq[T]] = tss.sortWith((a: Seq[T], b: Seq[T]) => { _freq(a.length, tss) < _freq(b.length, tss) }: Boolean)
}
