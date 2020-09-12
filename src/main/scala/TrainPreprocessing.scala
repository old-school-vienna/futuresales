import java.io.FileNotFoundException

import scala.io.Source
import scala.util.Try

object TrainPreprocessing {

  def read(): Seq[Train] = {
    try {
      readFromCache()
    } catch {
      case _: FileNotFoundException =>
        val tr = readTrain()
        writeToCache(tr)
        tr
    }
  }

  private def readFromCache(): Seq[Train] = ???

  private def writeToCache(iterable: Iterable[Train]): Unit = ???

  private def readCategories(filename: String): Map[Int, Int] = {

    def readIds(line: Array[String]): (Int, Int) = {
      val len = line.length
      try {
        (line(len - 2).toInt, line(len - 1).toInt)
      } catch {
        case e: Exception =>
          val m = line.toList.mkString("(", ",", ")")
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

  private def readSalesTrainCsv[K, T](
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

  private def readTrain(): Seq[Train] = {

    def toMonthShopItemSales(sales: Iterable[SalesTrain]): Train = {
      val sSeq = sales.toSeq
      val first = sSeq.head
      Train(
        month = first.month,
        shop_id = first.shop_id,
        item_id = first.item_id,
        item_price = sSeq.map(_.item_price).sum / sSeq.size,
        item_cnt = sSeq.map(_.item_cnt_day).sum,
        cat_id = first.cat_id,
      )
    }

    def monthShopItemKey(st: SalesTrain): Ordered[_] = (st.month, st.item_id, st.shop_id)

    val catMap = readCategories("data/items.csv")

    readSalesTrainCsv("data/sales_train.csv", monthShopItemKey, toMonthShopItemSales, catMap)
      .toList
      .sortBy(s => (s.item_id, s.shop_id, s.month))
  }

}
