import scala.io.Source

case class SalesTrain(
                       date: String,
                       month: Int,
                       shop_id: String,
                       item_id: String,
                       item_price: Double,
                       item_cnt_day: Double
                     )

case class SalesTrainShopItem(
                               month: Int,
                               shop_id: String,
                               item_id: String,
                               item_price: Double,
                               item_cnt_day: Double
                             )


object Analyse extends App {

  def toSailsTrainItem(sales: Iterable[SalesTrain]): SalesTrainShopItem = {
    val sSeq = sales.toSeq
    val first = sSeq.head
    SalesTrainShopItem(
      month = first.month,
      shop_id = first.shop_id,
      item_id = first.item_id,
      item_price = sSeq.map(_.item_price).sum / sSeq.size,
      item_cnt_day = sSeq.map(_.item_cnt_day).sum,
    )
  }

  def toSailsTrain(line: Array[String]): SalesTrain = {
    SalesTrain(
      line(0), line(1).toInt, line(2), line(3),
      line(4).toDouble, line(5).toDouble)
  }

  def groupCsv[O, T](
                      filename: String,
                      fGrp: SalesTrain => O,
                      fAgg: Iterable[SalesTrain] => T): Iterable[T] = {
    val src = Source.fromFile(filename)
    try {
      src.getLines()
        .drop(1)
        .map(_.split(","))
        .map(toSailsTrain)
        .to(LazyList)
        .groupBy(st => fGrp(st))
        .toList
        .map(t => fAgg(t._2))
    } finally {
      src.close()
    }
  }

  def k1(st: SalesTrain): Ordered[_] = (st.month, st.item_id, st.shop_id)

  groupCsv("data/sales_train.csv", k1, toSailsTrainItem)
    .toList
    .sortBy(s => (s.item_id, s.shop_id, s.month))
    .foreach(println(_))


}