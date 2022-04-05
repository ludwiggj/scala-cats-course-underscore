package scala_cats.casestudies.cdrt.take3

object Helpers {
  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(k: K, v: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(k, v)

    def get(k: K)(implicit kvs: KeyValueStore[F]): Option[V] =
      kvs.get(f)(k)

    def getOrElse(k: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(k, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] =
      kvs.values(f)
  }
}
