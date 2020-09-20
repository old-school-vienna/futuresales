import Util.{Situation, readCsv}

object DataProvider {

  private lazy val salesTrainFull = _readSalesTrainFull

  private lazy val salesTrainLocal = _readSalesTrainLocal

  private lazy val testData = _readTestData()

  private case class SalesTrain(
                                 date: String,
                                 month: Int,
                                 shopId: Int,
                                 itemId: Int,
                                 itemPrice: Double,
                                 itemCntDay: Double,
                                 catId: Int,
                               )

  private def toTestDs(line: Array[String]): TestDs = {
    TestDs(id = line(0).toInt,
      shopItemId = ShopItemId(line(1).toInt, line(2).toInt),
    )
  }


  def readTestData(): Seq[TestDs] = testData

  private def _readTestData(): Seq[TestDs] = {
    println("read test data")
    readCsv("data/test.csv", toTestDs)
  }

  def readSalesTrain(situation: Situation): Seq[TrainDs] = {
    situation match {
      case Situation.Full => this.salesTrainFull
      case Situation.Local => this.salesTrainLocal
    }
  }

  private def _readSalesTrainLocal: Seq[TrainDs] = {
    println(s"reading sales train local")
    this.salesTrainFull.filter(x => x.month <= 32)
  }

  private def _readSalesTrainFull: Seq[TrainDs] = {
    println(s"reading sales train full")
    def readSalesTrainCsv(filename: String,
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

      def toMonthShopItemSales(sales: Iterable[SalesTrain]): Option[TrainDs] = {
        val sSeq = sales.toSeq
        sSeq match {
          case Nil => None
          case ss =>
            val first = ss.head
            Some(TrainDs(
              month = first.month,
              shopItemId = ShopItemId(first.shopId, first.itemId),
              catId = first.catId,
              itemPrice = sSeq.map(_.itemPrice).sum / sSeq.size,
              itemCnt = sSeq.map(_.itemCntDay).sum,
            ))
        }
      }

      Util.readCsv(filename, toSailsTrain)
        .filter(x => x.itemId != 11373 && x.shopId != 12)
        .groupBy(st => (st.month, st.itemId, st.shopId, st.catId))
        .flatMap(t => toMonthShopItemSales(t._2))
        .toSeq
    }


    def readCategories(filename: String): Map[Int, Int] = {

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

    val catMap = readCategories("data/items.csv")
    readSalesTrainCsv(
      filename = "data/sales_train.csv",
      catMapping = catMap)
  }

}
