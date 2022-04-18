package scala_cats.casestudies.cdrt.take1

final case class GCounterTextbook(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): GCounterTextbook = {
    val newAmount = counters.getOrElse(machine, 0) + amount
    GCounterTextbook(counters + (machine -> newAmount))
  }

  def merge(that: GCounterTextbook): GCounterTextbook =
    GCounterTextbook(that.counters ++ this.counters.map {
      case (k, v) =>
        k -> (v max that.counters.getOrElse(k, 0))
    })

  def total: Int =
    counters.values.sum
}
