package scala_cats.chapter07

object FoldingWorkout {
  def show[A](list: List[A]): String = list.foldLeft("nil")((accum, item) => s"$item then $accum")
}