package org.eigengo.stw.tesco

import scala.io.Source
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport

object Offline {

  lazy val products: List[Product] = {
    val is = Source.fromInputStream(Offline.getClass.getResourceAsStream("/booze.txt"))
    // Use is.getLines() to get Iterator[String] representing each line
    // Convert each line to a Product
    // And turn the result to List[Product]
    // N.B. some lines are malformed and do not contain three elements

    val products = is.getLines().map(_.split("\\|")).filter(_.length == 3).map { elements =>
      Product(elements(0), BigDecimal(elements(1)), elements(2))
    }
    products.toList
  }

  def search(query: String): List[Product] = products.filter(_.Name contains query)

  def main(a: Array[String]): Unit = println(search("beer"))

}

object OfflineConversions extends App with DefaultJsonProtocol with SprayJsonSupport {
  import spray.httpx.unmarshalling._
  import spray.http._
  implicit val ProductFormat = jsonFormat3(Product)
  implicit val ProductSearchFormat = jsonFormat2(ProductSearchResponse)

  def convert(): Unit = {

    def convertProduct(product: Product): String = s"${product.Name}|${product.Price}|${product.ShelfCategoryName}"

    val body = HttpEntity(ContentTypes.`application/json`, Source.fromFile("/Users/janmachacek/booze.json").mkString)
    body.as[ProductSearchResponse] match {
      case Right(psr) =>
        val newContent = (psr.Products map convertProduct).mkString("\n")
        println(newContent)
    }

  }

  convert()

}
