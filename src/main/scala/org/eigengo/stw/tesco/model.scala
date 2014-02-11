package org.eigengo.stw.tesco

case class Product(Name: String, Price: BigDecimal, ShelfCategoryName: String)
case class ProductSearchResponse(StatusCode: Int, Products: List[Product])

object commands {
  val ProductSearch = "PRODUCTSEARCH"
  val ListProductOffers = "LISTPRODUCTOFFERS"
}