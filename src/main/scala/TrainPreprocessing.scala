import java.io.FileNotFoundException

object TrainPreprocessing {

  private val cacheFilename = "data/sales_train_enriched.csv"
  private val separator = ","

  case class SalesTrain(
                         date: String,
                         month: Int,
                         shopId: Int,
                         itemId: Int,
                         itemPrice: Double,
                         itemCntDay: Double,
                         catId: Int,
                       )

  def read(): Seq[Train] = {
    try {
      val tr = readFromCache()
      println("Successfully read from cache")
      tr
    } catch {
      case _: FileNotFoundException =>
        val tr = readTrain()
        println("Successfully read from original")
        writeToCache(tr)
        println("Successfully wrote to cache")
        tr
    }
  }


  private def readFromCache(): Seq[Train] = {

    def lineToTrain(line: Array[String]): Train = {
      Train(month = line(0).toInt,
        shop_id = line(1).toInt,
        item_id = line(2).toInt,
        item_price = line(3).toDouble,
        item_cnt = line(4).toDouble,
        cat_id = line(5).toInt,
      )
    }

    Util.readCsv(cacheFilename, lineToTrain, headerLines = 0)
  }

  private def writeToCache(trains: Iterable[Train]): Unit = {

    def trainToLine(t: Train): String = {
      val sb = new StringBuilder
      sb.append(t.month)
      sb.append(separator)
      sb.append(t.shop_id)
      sb.append(separator)
      sb.append(t.item_id)
      sb.append(separator)
      sb.append(t.item_price)
      sb.append(separator)
      sb.append(t.item_cnt)
      sb.append(separator)
      sb.append(t.cat_id)

      sb.toString()
    }

    Util.writeCsv(cacheFilename, trains, trainToLine)
  }

  private def readCategories(filename: String): Map[Int, Int] = {

    def readIds(line: Array[String]): (Int, Int) = {
      val len = line.length
      try {
        (line(len - 2).toInt, line(len - 1).toInt)
      } catch {
        case _: Exception =>
          val m = line.toList.mkString("(", ",", ")")
          throw new IllegalArgumentException(s"Could not read $m")
      }
    }

    Util.readCsv(filename, readIds).toMap
  }

  private def readSalesTrainCsv[K, T](
                                       filename: String,
                                       fKey: SalesTrain => K,
                                       fAgg: Iterable[SalesTrain] => T,
                                       catMapping: Map[Int, Int]): Seq[T] = {

    def toSailsTrain(line: Array[String]): SalesTrain = {
      SalesTrain(
        date = line(0),
        month = line(1).toInt,
        shopId = line(2).toInt,
        itemId = line(3).toInt,
        itemPrice= line(4).toDouble,
        itemCntDay = line(5).toDouble,
        catId = catMapping(line(3).toInt))
    }

    Util.readCsv(filename, toSailsTrain)
      .to(LazyList)
      .groupBy(st => fKey(st))
      .toSeq
      .map(t => fAgg(t._2))
  }

  private def readTrain(): Seq[Train] = {

    def toMonthShopItemSales(sales: Iterable[SalesTrain]): Train = {
      val sSeq = sales.toSeq
      val first = sSeq.head
      Train(
        month = first.month,
        shop_id = first.shopId,
        item_id = first.itemId,
        cat_id = first.catId,
        item_price = sSeq.map(_.itemPrice).sum / sSeq.size,
        item_cnt = sSeq.map(_.itemCntDay).sum,
      )
    }

    def monthShopItemKey(st: SalesTrain): Ordered[_] = (st.month, st.itemId, st.shopId, st.catId)

    val catMap = readCategories("data/items.csv")

    readSalesTrainCsv(
      filename= "data/sales_train.csv",
      fKey= monthShopItemKey,
      fAgg = toMonthShopItemSales,
      catMapping = catMap)
  }

}
