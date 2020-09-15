import java.io.FileNotFoundException

import Util.Situation

object TrainPreprocessing {

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

  def read(situation: Situation = Situation.Full, caching: Boolean = true): Seq[TrainDs] = {
    try {
      if (caching) {
        val tr = readFromCache(situation)
        println(s"Successfully read from cache $situation")
        tr
      } else {
        val tr = readTrain(situation)
        println(s"Caching disabled. Successfully read from original $situation")
        tr
      }
    } catch {
      case _: FileNotFoundException =>
        val tr = readTrain(situation)
        println(s"Successfully read from original $situation")
        writeToCache(tr, situation)
        println(s"Successfully wrote to cache $situation")
        tr
    }
  }


  private def readFromCache(situation: Situation): Seq[TrainDs] = {

    def lineToTrain(line: Array[String]): TrainDs = {
      TrainDs(month = line(0).toInt,
        shopItemId = ShopItemId(line(1).toInt, line(2).toInt),
        itemPrice = line(3).toDouble,
        itemCnt = line(4).toDouble,
        catId = line(5).toInt,
      )
    }

    val full = Util.readCsv(filename(situation), lineToTrain, headerLines = 0)
    situation match {
      case Situation.Full =>full
      case Situation.Local =>full.filter(t => t.month != 33)
    }
  }

  private def writeToCache(trains: Iterable[TrainDs], situation: Situation): Unit = {

    def trainToLine(t: TrainDs): String = {
      val sb = new StringBuilder
      sb.append(t.month)
      sb.append(separator)
      sb.append(t.shopItemId.shopId)
      sb.append(separator)
      sb.append(t.shopItemId.itemId)
      sb.append(separator)
      sb.append(t.itemPrice)
      sb.append(separator)
      sb.append(t.itemCnt)
      sb.append(separator)
      sb.append(t.catId)

      sb.toString()
    }

    Util.writeCsv(filename(situation), trains, trainToLine)
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

  private def readSalesTrainCsv(
                                 filename: String,
                                 catMapping: Map[Int, Int]): Seq[TrainDs] = {

    def toSailsTrain(line: Array[String]): SalesTrain = {
      SalesTrain(
        date = line(0),
        month = line(1).toInt,
        shopId = line(2).toInt,
        itemId = line(3).toInt,
        itemPrice = line(4).toDouble,
        itemCntDay = line(5).toDouble,
        catId = catMapping(line(3).toInt))
    }

    def toMonthShopItemSales(sales: Iterable[SalesTrain]): TrainDs = {
      val sSeq = sales.toSeq
      val first = sSeq.head
      TrainDs(
        month = first.month,
        shopItemId = ShopItemId(first.shopId, first.itemId),
        catId = first.catId,
        itemPrice = sSeq.map(_.itemPrice).sum / sSeq.size,
        itemCnt = sSeq.map(_.itemCntDay).sum,
      )
    }

    Util.readCsv(filename, toSailsTrain)
      .groupBy(st => (st.month, st.itemId, st.shopId, st.catId))
      .map(t => toMonthShopItemSales(t._2))
      .toSeq
  }

  private def readTrain(situation: Situation): Seq[TrainDs] = {
    val catMap = readCategories("data/items.csv")
    readSalesTrainCsv(
      filename = "data/sales_train.csv",
      catMapping = catMap)
  }

  private def filename(situation: Situation) = {
    situation match {
      case Situation.Full => "data/sales_train_cached_full.csv"
      case Situation.Local => "data/sales_train_cached_local.csv"
    }
  }
}
