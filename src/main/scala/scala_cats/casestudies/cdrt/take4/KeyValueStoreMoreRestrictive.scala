package scala_cats.casestudies.cdrt.take4

trait KeyValueStoreMoreRestrictive[F[_, _], K, V] {
  def put(f: F[K, V])(k: K, v: V): F[K, V]

  def get(f: F[K, V])(k: K): Option[V]

  def getOrElse(f: F[K, V])(k: K, default: V): V =
    get(f)(k).getOrElse(default)

  def values(f: F[K, V]): List[V]
}

object KeyValueStoreMoreRestrictive {
  implicit val mapInstance: KeyValueStoreMoreRestrictive[Map, String, Int] = new KeyValueStoreMoreRestrictive[Map, String, Int] {
    override def put(f: Map[String, Int])(k: String, v: Int): Map[String, Int] = f + (k -> v)

    override def get(f: Map[String, Int])(k: String): Option[Int] = f.get(k)

    override def values(f: Map[String, Int]): List[Int] = f.values.toList
  }

  def apply[F[_, _], K, V](implicit rkvs: KeyValueStoreMoreRestrictive[F, K, V]): KeyValueStoreMoreRestrictive[F, K, V] = rkvs
}

