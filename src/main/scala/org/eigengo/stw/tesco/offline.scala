package org.eigengo.stw.tesco

import scala.io.Source

object Offline {

  lazy val byCategory: Map[String, List[Product]] = ???

  lazy val products: List[Product] = {
    val is = Source.fromInputStream(Offline.getClass.getResourceAsStream("/booze.txt"))
    // Use is.getLines() to get Iterator[String] representing each line
    // Convert each line to a Product
    // And turn the result to List[Product]
    // N.B. some lines are malformed and do not contain three elements (oopsie! :))

    val products = is.getLines().map(_.split('|')).filter(_.length == 3).map { elements =>
      Product(elements(0), BigDecimal(elements(1)), elements(2))
    }
    products.toList
  }

  def search(query: String): List[Product] = ???

  def mostExpensive(query: String): Product = ???

  def cheapest(query: String): Product = ???

}
