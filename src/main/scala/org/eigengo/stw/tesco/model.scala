package org.eigengo.stw.tesco

case class Product(name: String, price: BigDecimal, shelfCategoryName: String)
case class ProductSearchResponse(statusCode: Int, products: List[Product])

object commands {
  val ProductSearch = "PRODUCTSEARCH"
}