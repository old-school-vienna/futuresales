import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}

import scala.io.Source


object TrainPreprocessing {

  private val cacheFilename = "data/sales_train_enriched.csv"
  private val separator = ","

  case class SalesTrain(
                         date: String,
                         month: Int,
                         shop_id: Int,
                         item_id: Int,
                         item_price: Double,
                         item_cnt_day: Double,
                         cat_id: Int,
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

    val src = Source.fromFile(cacheFilename)
    try {
      src.getLines()
        .map(_.split(separator))
        .map(lineToTrain)
        .toSeq
    } finally {
      src.close()
    }
  }

  private def writeToCache(trains: Iterable[Train]): Unit = {


    def writeFile(filename: String, lines: Iterable[String]): Unit = {
      val file = new File(filename)
      val bw = new BufferedWriter(new FileWriter(file))
      try {
        for (line <- lines) {
          bw.write(line)
        }
      } finally {
        bw.close()
      }
    }

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
      sb.append("\n")

      sb.toString()
    }

    val strLines: Iterable[String] = trains.map(trainToLine)
    writeFile(cacheFilename, strLines)
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

    val src = Source.fromFile(filename)
    try {
      src.getLines()
        .drop(1)
        .map(_.split(separator))
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
  }

}
