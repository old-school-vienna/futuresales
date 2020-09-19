import Util.Situation

object Tryout extends App {
  
def calcValueAt33(): Unit = {
  val ids = Seq(
    ShopItemId(31, 20949), // 431.0 (ShopItemId(31,20949),List(TrainDs(33,ShopItemId(31,20949),4.9973118279570965,431.0,71)))
    ShopItemId(25, 20949), // 461.0 (ShopItemId(25,20949),List(TrainDs(33,ShopItemId(25,20949),5.0,461.0,71)))
  )

  DataProvider.readSalesTrain(situation = Situation.Full)
    .filter(t => t.month == 33)
    .filter(t => ids.contains(t.shopItemId))
    .groupBy(t => t.shopItemId)
    .toSeq
    .sortBy(t => t._1.itemId)
    .sortBy(t => t._1.shopId)
    .foreach(println(_))
}

  def showMonths(): Unit ={
    val months = DataProvider.readSalesTrain(situation = Situation.Full)
      .groupBy(t => t.month)
      .map(t => t._1)
      .toSeq
      .sorted
      .mkString(", ")

    println(s"months: $months")
  }

showMonths()  
//calcValueAt33()
}
