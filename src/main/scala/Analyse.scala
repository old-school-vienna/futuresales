import scala.io.Source

case class SalesTrain(
                       date: String,
                       month: Int,
                       shop_id: Int,
                       item_id: Int,
                       item_price: Double,
                       item_cnt_day: Double,
                       cat_id: Int,
                     )

case class MonthShopItemSales(
                               month: Int,
                               shop_id: Int,
                               item_id: Int,
                               item_price: Double,
                               item_cnt_day: Double,
                               cat_id: Int,
                             )


object Analyse extends App {

  def readCategories(filename: String): Map[Int, Int] = {

    def readIds(line: Array[String]): (Int, Int) = {
      val len = line.length
      try {
        (line(len - 2).toInt, line(len - 1).toInt)
      } catch {
        case e: Exception =>
          val m = line.toList.mkString("(",",",")")
          throw new IllegalArgumentException(s"Could not read $m")
      }
    }

    val src = Source.fromFile(filename)
    try {
      src.getLines()
        .drop(1)
        .map(_.split(","))
        .map(readIds)
        .toMap
    } finally {
      src.close()
    }
  }

  def readSalesTrainCsv[K, T](
                               filename: String,
                               fKey: SalesTrain => K,
                               fAgg: Iterable[SalesTrain] => T,
                               catMapping: Map[Int, Int]): Seq[T] = {

    def toSailsTrain(line: Array[String]): SalesTrain = {
      SalesTrain(
        line(0), line(1).toInt, line(2).toInt, line(3).toInt,
        line(4).toDouble, line(5).toDouble, catMapping(line(3).toInt))
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

  private def readTrain(): Unit = {
    def toMonthShopItemSales(sales: Iterable[SalesTrain]): MonthShopItemSales = {
      val sSeq = sales.toSeq
      val first = sSeq.head
      MonthShopItemSales(
        month = first.month,
        shop_id = first.shop_id,
        item_id = first.item_id,
        item_price = sSeq.map(_.item_price).sum / sSeq.size,
        item_cnt_day = sSeq.map(_.item_cnt_day).sum,
        cat_id = first.cat_id,
      )
    }

    def monthShopItemKey(st: SalesTrain): Ordered[_] = (st.month, st.item_id, st.shop_id)

    val catMap = readCategories("data/items.csv")

    val msis = readSalesTrainCsv("data/sales_train.csv", monthShopItemKey, toMonthShopItemSales, catMap)
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

    println()
    msis.take(10).foreach(m => println(m))
  }

  readTrain()
}