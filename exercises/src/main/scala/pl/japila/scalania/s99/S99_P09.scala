package pl.japila.scalania.s99

object S99_P09 {

  def merge[T](ts: Seq[Seq[T]]): Seq[Seq[T]] =
    ts match {
      case Nil => Nil
      case a :: Nil => Seq(a)
      case (a :: a2) :: ((b :: b2) :: c) => if (a == b) merge(((a :: a2) ++ (b :: b2)) :: c)
      else Seq((a :: a2)) ++ merge((b :: b2) :: c)
    }

  def pack[T](ts: Seq[T]): Seq[Seq[T]] =

    merge(ts match {
      case Nil => Nil
      case a :: Nil => Seq(Seq(a))
      case a :: (b :: c) => if (a == b) Seq(Seq(a, b)) ++ pack(c)
      else Seq(Seq(a)) ++ pack(b :: c)
    })
}
