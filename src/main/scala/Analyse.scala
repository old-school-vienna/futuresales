import scala.io.Source

case class SalesTrain(
                       date: String,
                       month: Int,
                       shop_id: Int,
                       item_id: Int,
                       item_price: Double,
                       item_cnt_day: Double
                     )

case class MonthShopItemSales(
                               month: Int,
                               shop_id: Int,
                               item_id: Int,
                               item_price: Double,
                               item_cnt_day: Double
                             )


object Analyse extends App {

  def groupCsv[K, T](
                      filename: String,
                      fKey: SalesTrain => K,
                      fAgg: Iterable[SalesTrain] => T): Seq[T] = {

    def toSailsTrain(line: Array[String]): SalesTrain = {
      SalesTrain(
        line(0), line(1).toInt, line(2).toInt, line(3).toInt,
        line(4).toDouble, line(5).toDouble)
    }

    val src = Source.fromFile(filename)
    try {
      src.getLines()
        .drop(1)
        .map(_.split(","))
        .map(toSailsTrain)
        .to(LazyList)
        .groupBy(st => fKey(st))
        .toSeq
        .map(t => fAgg(t._2))
    } finally {
      src.close()
    }
  }

  def toMonthShopItemSales(sales: Iterable[SalesTrain]): MonthShopItemSales = {
    val sSeq = sales.toSeq
    val first = sSeq.head
    MonthShopItemSales(
      month = first.month,
      shop_id = first.shop_id,
      item_id = first.item_id,
      item_price = sSeq.map(_.item_price).sum / sSeq.size,
      item_cnt_day = sSeq.map(_.item_cnt_day).sum,
    )
  }

  def monthShopItemKey(st: SalesTrain): Ordered[_] = (st.month, st.item_id, st.shop_id)

  val msis = groupCsv("data/sales_train.csv", monthShopItemKey, toMonthShopItemSales)
    .toList
    .sortBy(s => (s.item_id, s.shop_id, s.month))

  val months = msis.groupBy(m => m.month).keys.toList.sorted
  println(s"months: ${months.size} [${months.mkString(",")}]")
  val items = msis.groupBy(m => m.item_id).keys.toList.sorted
  val il = items.take(10).mkString(",")
  val ir = items.takeRight(10).mkString(",")
  println(s"items: ${items.size} [$il ... $ir]")
  val shops = msis.groupBy(m => m.shop_id).keys.toList.sorted
  println(s"shops: ${shops.size} [${shops.mkString(",")}]")
}