package DataStructures

object Homework extends App {
  // Implement a special sort which sorts the keys of a map (K) according to their associated
  // values (V).
  //
  // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
  //
  // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
  // values.
  //
  // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
  // with values from K, and the associated value V for these values in the `Set`.
  //
  // For example:
  //
  // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
  // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.

  // Fixed old version
  // def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
  //   List(
  //     map
  //       .groupMap(_._2)(_._1)
  //       .map(_.swap)
  //       .map({ case (k, v) => (Set(k.toSeq: _*).to(Set), v) })
  //       .toSeq: _*
  //   ).sorted((a: (Set[T], Int), b: (Set[T], Int)) => a._2 - b._2)
  // }

  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    map
      .groupBy { case (_, v) => v }
      .map(_.swap)
      .map { case (k, v) => (k.map { case (set, _) => set }.toSet, v) }
      .toList
      .sorted((a: (Set[T], Int), b: (Set[T], Int)) => a._2 - b._2)
  }
}
