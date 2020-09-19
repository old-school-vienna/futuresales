import Util.Situation

object Tryout extends App {

  val ids = Seq(
    ShopItemId(25, 20949),
    ShopItemId(28, 20949),
    ShopItemId(42,20949),
    ShopItemId(12,11373),
    ShopItemId(12,11370),
  )
  /*
(ShopItemId(31,20949),410.0))
((ShopItemId(25,20949),997.0),(ShopItemId(25,20949),410.0))
((ShopItemId(28,20949),739.0),(ShopItemId(28,20949),200.0))
((ShopItemId(42,20949),687.0),(ShopItemId(42,20949),480.0))
((ShopItemId(12,11373),265.0),(ShopItemId(12,11373),400.0))
((ShopItemId(12,11370),601.0),(ShopItemId(12,11370),200.0))
((ShopItemId(57,20949),452.0),(ShopItemId(57,20949),100.0))
((ShopItemId(47,20949),404.0),(ShopItemId(47,20949),150.0))
((ShopItemId(22,20949),177.0),(ShopItemId(22,20949),50.0))
((ShopItemId(21,20949),280.0),(ShopItemId(21,20949),150.0))
((ShopItemId(46,20949),167.0),(ShopItemId(46,20949),100.0))
*/

  DataProvider.readSalesTrain(situation = Situation.Full)
    .filter(t => t.month == 33)
    .filter(t => ids.contains(t.shopItemId))
    .groupBy(t => t.shopItemId)
    .toSeq
    .sortBy(t => t._1.itemId)
    .sortBy(t => t._1.shopId)
    .foreach(println(_))


}
