import Util.{Situation, proposedValuesMean, toi}
import entelijan.viz.Viz.XY

object DfTrain {


  /*
  df_train.csv
  "shop_id","item_id","month_nr","cnt","cnt_shop","cnt_item","cnt1","cnt2","cnt3","cnt4","cnt5","cnt6","cnt_3m","cnt_6m","cnt_shop1","cnt_shop2","cnt_shop3","cnt_shop_3m","cnt_item1","cnt_item2","cnt_item3","cnt_item_3m"
2,1495,0,0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
2,1495,1,0,0,0,0,NA,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,0,NA,NA,NA
2,1495,2,0,0,0,0,0,NA,NA,NA,NA,NA,NA,0,0,NA,NA,0,0,NA,NA
2,1495,3,0,0,0,0,0,0,NA,NA,NA,0,NA,0,0,0,0,0,0,0,0
2,1495,4,0,0,0,0,0,0,0,NA,NA,0,NA,0,0,0,0,0,0,0,0
2,1495,5,0,0,0,0,0,0,0,0,NA,0,NA,0,0,0,0,0,0,0,0
2,1495,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
2,1495,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
   */

  private case class Train(
                            shopItemId: ShopItemId,
                            monthNr: Int,
                            cnt: Int,
                            cntShop: Int,
                            cntItem: Int,
                            cnt1: Int,
                            cnt2: Int,
                            cnt3: Int,
                            cnt4: Int,
                            cnt5: Int,
                            cnt6: Int,
                            cnt3m: Int,
                            cnt6m: Int,
                            cntShop1: Int,
                            cntShop2: Int,
                            cntShop3: Int,
                            cntShop3m: Int,
                            cntItem1: Int,
                            cntItem2: Int,
                            cntItem3: Int,
                            cntItem3m: Int
                          )

  private def toTrain(line: Array[String]): Train = {
    Train(
      shopItemId = ShopItemId(toi(line(0)), toi(line(1))),
      monthNr = toi(line(2)),
      cnt = toi(line(3)),
      cntShop = toi(line(4)),
      cntItem = toi(line(5)),
      cnt1 = toi(line(6)),
      cnt2 = toi(line(7)),
      cnt3 = toi(line(8)),
      cnt4 = toi(line(9)),
      cnt5 = toi(line(10)),
      cnt6 = toi(line(11)),
      cnt3m = toi(line(12)),
      cnt6m = toi(line(13)),
      cntShop1 = toi(line(14)),
      cntShop2 = toi(line(15)),
      cntShop3 = toi(line(16)),
      cntShop3m = toi(line(17)),
      cntItem1 = toi(line(18)),
      cntItem2 = toi(line(19)),
      cntItem3 = toi(line(20)),
      cntItem3m = toi(line(21)),
    )
  }

  private def toDoubleArray(train: Train): Seq[Double] = {
    val result: Array[Double] = Array.fill(20)(0.0)
    result(0) = train.monthNr
    result(1) = train.cnt
    result(2) = train.cntShop
    result(3) = train.cntItem
    result(4) = train.cnt1
    result(5) = train.cnt2
    result(6) = train.cnt3
    result(7) = train.cnt4
    result(8) = train.cnt5
    result(9) = train.cnt6
    result(10) = train.cnt3m
    result(11) = train.cnt6m
    result(12) = train.cntShop1
    result(13) = train.cntShop2
    result(14) = train.cntShop3
    result(15) = train.cntShop3m
    result(16) = train.cntItem1
    result(17) = train.cntItem2
    result(18) = train.cntItem3
    result(19) = train.cntItem3m
    result.toSeq
  }

  private def predictors(data: Iterable[Train]): Iterable[Double] = {
    data
      .filter(t => t.monthNr > 10)
      .filter(t => t.monthNr < 34)
      .flatMap(toDoubleArray)
  }

  private def truth(id: ShopItemId): Double = {
    val sid = Util.shopItemIdToSubmissionId(id).get
    LocalTester.truthMap.getOrElse(sid, 0.0)
  }

  private lazy val meanMap: Map[ShopItemId, Double] = {
    val situation = Situation.Local
    val trainData: Map[ShopItemId, Seq[TrainDs]] = DataProvider.readSalesTrain(situation)
      .groupBy(st => st.shopItemId)
    proposedValuesMean(trainData, situation)
  }

  private def mean(shopItemId: ShopItemId): Double = {
    meanMap.getOrElse(shopItemId, 0.0)
  }

  def analyseTrain(): Unit = {

    /*
    ShopItemId(31,1201)         35 
    ShopItemId(55,20956)        35 
    ShopItemId(56,1495)         35 
    ShopItemId(59,19415)        35 
    ShopItemId(57,5823)         35 
    
    Found length of every shop/item is 35
     */
    val filename = Util.inputDirectory.resolve("df_train.csv")
    val all = Util.readCsv(filename, toTrain)
      .groupBy(t => t.shopItemId)
      .map { case (id, seq) => (id, truth(id), predictors(seq)) }
      .filter(t => t._2 > 0.00001)
      .toSeq
      .sortBy(t => t._2)


    val filtered = all
      .filter(st => !Util.errorItemsSituationLocal.contains(ShopItemId(st._1.shopId, st._1.itemId)))

    println(f"all       size : ${all.size}%10d")
    println(f"filtered  size : ${filtered.size}%10d")
    println(f"predictor size : ${all(0)._3.size}%10d")
  }

  def printMonths(): Unit = {

    val filename = Util.inputDirectory.resolve("df_train.csv")
    Util.readCsv(filename, toTrain)
      .groupBy(t => t.shopItemId)
      .map { case (id, seq) => (id, truth(id), mean(id), seq) }
      .toSeq
      .sortBy { case (_, _, mean, _) => mean }
      .sortBy { case (_, truth, _, _) => truth }
      .foreach { case (id, truth, _, seq) =>
        val month = seq
          .map(t => t.monthNr)
          .sorted
          .mkString(",")
        println(f"$id $truth --- $month")
      }
  }

  def plotCount(): Unit = {

    sealed trait Location

    object Location {

      case object Top extends Location

      case object Bottom extends Location

    }

    def minMax[T](all: Seq[(Double, Double, T)]): (Int, Int, Double, Double, Seq[T]) = {
      val ts = all.map { case (t, _, _) => t.toInt }
      val tm = all.map { case (_, m, _) => m }
      val seq = all.map { case (_, _, ys) => ys }
      (ts.min, ts.max, tm.min, tm.max, seq)
    }

    val grpSize = 20

    def plot(counts: Seq[(Double, Double, Seq[(Int, Int)])], location: Location): Unit = {

      val counts1 = location match {
        case Location.Top =>
          counts.take(grpSize * 50)
        case Location.Bottom =>
          counts.takeRight(grpSize * 50)
      }
      val counts2 = counts1
        .grouped(grpSize)
        .toSeq
        .map(minMax)

      import entelijan.vizb._

      val id = location match {
        case Location.Top => "top"
        case Location.Bottom => "bottom"
      }
      MultiChartBuilder(s"df_train_counts_$id")
        .title(s"sales for $grpSize shop/item on every diagram with similar truth - $id")
        .columns(5)
        .size(2000, 4000)
        .buildables(counts2.map {
          case (truthMin, truthMax, meanMin, meanMax, count) =>
            LineChartBuilder()
              .title(f"min/max truth $truthMin/$truthMax mean $meanMin%.2f/$meanMax%.2f")
              .yRangeMin(0)
              .creatables(count.map(xys =>
                DataRowBuilder()
                  .data(xys.map { case (y, x) => XY(x, y) })
                  .build()))
        })
        .create()
    }

    val filename = Util.inputDirectory.resolve(s"df_train.csv")
    val counts: Seq[(Double, Double, Seq[(Int, Int)])] = Util.readCsv(filename, toTrain)
      .filter(t => !Util.errorItemsSituationLocal.contains(t.shopItemId))
      .groupBy(t => t.shopItemId)
      .map { case (id, seq) => (id, truth(id), mean(id), seq) }
      .toSeq
      .sortBy { case (_, _, mean, _) => -mean }
      .sortBy { case (_, truth, _, _) => -truth }
      .map { case (_, truth, mean, seq) =>
        (truth, mean, seq
          .filter(t => t.monthNr < 34)
          .map(t => t.cnt)
          .zipWithIndex)
      }

    plot(counts, Location.Top)
    plot(counts, Location.Bottom)
  }

}
