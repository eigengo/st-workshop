package org.eigengo.stw.tesco

import scala.io.Source

object Offline {

  lazy val byCategory: Map[String, List[Product]] = ???

  lazy val products: List[Product] = {
    val is = Source.fromInputStream(Offline.getClass.getResourceAsStream("/booze.txt"))
    // Use ``is.getLines()`` to get ``Iterator[String]`` representing each line.
    // Convert each line to a ``Product`` (the format is ``name '|' price '|' category``), where
    // ``name`` and ``category`` are ``String``s, and ``price`` is ``BigDecimal``.
    // Consume all lines by converting the result to ``List[Product]``.
    //
    // N.B. some lines are malformed and do not contain three elements (oopsie! :))
    ???
  }

  def search(query: String): List[Product] = ???

  def mostExpensive(query: String): Product = ???

  def cheapest(query: String): Product = ???

}
