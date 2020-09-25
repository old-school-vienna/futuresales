import java.util.Locale

import Util.Situation

object DataExport extends App {

  exportShopItemMonth(Situation.Local)

  def exportShopItemMonth(situation: Situation): Unit = {

    val separator = ","

    def f(value: Double): String = "%.1f".formatLocal(Locale.ENGLISH, value)

    def trainDsToString(t: (Int, TrainDs)): String = {
      val sb = new StringBuilder
      sb.append(t._1)
      sb.append(separator)
      sb.append(t._2.shopItemId.shopId)
      sb.append(separator)
      sb.append(t._2.shopItemId.itemId)
      sb.append(separator)
      sb.append(t._2.catId)
      sb.append(separator)
      sb.append(t._2.month)
      sb.append(separator)
      sb.append(f(t._2.itemPrice))
      sb.append(separator)
      sb.append(f(t._2.itemCnt))
      sb.toString()
    }

    val grouped = Util.trainDataGroupedByShopItemId(situation)
    val meanMap: Map[ShopItemId, Double] = Util.proposedValuesMean(grouped, situation, 0.8)

    val mapIndex: Map[ShopItemId, Int] = meanMap.toList.sortBy(-_._2).map(_._1).zipWithIndex.toMap

    val data: Seq[(Int, TrainDs)] = DataProvider.readSalesTrain(situation)
      .map(td => (meanMap.getOrElse(td.shopItemId, 0.0), mapIndex(td.shopItemId), td))
      .sortBy(t => t._3.month)
      .sortBy(t => t._3.shopItemId.itemId)
      .sortBy(t => t._3.shopItemId.shopId)
      .sortBy(t => -t._1)
      .map(t => (t._2, t._3))
    val header = Seq("number", "shop_id", "item_id", "cat_id", "month", "item_price", "item_cnt").mkString(separator)
    Util.writeCsv("data/export_train_sales.csv", data, trainDsToString, Some(header))
  }


}
