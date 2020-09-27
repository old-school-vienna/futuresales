import Util.{Situation, createSubmission, proposedValuesMean, trainDataGroupedByShopItemId}
import entelijan.viz.Viz.XY
import entelijan.vizb.{Creatable, LineChartBuilder, MultiChartBuilder}

import scala.collection.parallel.CollectionConverters._

object StepwiseReal extends App {

  //runSimpleWithReal()
  //runMeanSorted()
  runVisualizeRegions()

  private def proposedSimpleCompleteSeq: Seq[(ShopItemId, Double)] = {
    val trainDataMap: Map[ShopItemId, Seq[TrainDs]] = trainDataGroupedByShopItemId(Situation.Local)
    val proposedSimpleMap: Map[ShopItemId, Double] = proposedValuesMean(trainDataMap, Situation.Local, 0.8)
    DataProvider.readTestData().map(td => (td.shopItemId, proposedSimpleMap.getOrElse(td.shopItemId, 0.0)))
  }


  def runMeanSorted(): Unit = {

    case class Cfg(name: String, yMax: Double, xMin: Double, xMax: Double)

    val xy = proposedSimpleCompleteSeq.sortBy(-_._2).map(_._2).zipWithIndex.map(t => XY(t._2, t._1))

    Seq(
      Cfg("overview", 500, 0, 200_000),
      Cfg("left20", 500, 0, 20),
      Cfg("left100", 500, 0, 100),
      Cfg("right20", 20, 0, 200_000),
      Cfg("right2", 2, 0, 200_000),
      Cfg("region1", 50, 0, 300),
      Cfg("region2", 0.5, 50_000, 160_000),
    ).foreach { cfg =>
      LineChartBuilder(s"mean_sorted_${cfg.name}")
        .yLabel("mean")
        .xLabel("shop/item")
        .title(s"mean sorted ${cfg.name}")
        .yRangeMax(cfg.yMax)
        .xRange(cfg.xMin, cfg.xMax)
        .data(xy)
        .create()
    }

  }


  def runVisualizeRegions(): Unit = {
    val all = DataProvider.readSalesTrain(Situation.Local)

    def toXy(shopItemId: ShopItemId): Seq[XY] = {
      val sales = all.filter(x => x.shopItemId == shopItemId)
      val vts: Map[Int, Double] = sales.map(t => (t.month, t.itemCnt)).toMap
      for (x <- 0 to 32) yield {
        val y: Double = vts.getOrElse(x, 0.0)
        XY(x.toDouble, y)
      }


    }


    val region1: Seq[(ShopItemId, Double, Int)] = proposedSimpleCompleteSeq
      .sortBy(-_._2)
      .zipWithIndex
      .filter(t => t._2 < 300)
      .map(t => (t._1._1, t._1._2, t._2))


    def multidia(data: Seq[(ShopItemId, Double, Int)], part: (Int, Int)): Unit = {

      val buildables = data.map {
        case (shopItemId, mean, _) =>
          val title = s"region 1 ${shopItemId.itemId}/${shopItemId.shopId} $mean%.2f"
          LineChartBuilder()
            .data(toXy(shopItemId))
            .title("title")


      }
      val sp = s"${part._1}_${part._2}"
      MultiChartBuilder(s"region_1_$sp")
        .title(s"Region 1 (${part._1} to ${part._2})")
        .buildables(buildables)
        .create()
    }

    Seq(
      (0, 8),
      (10, 18),
      (50, 58),
      (100, 108),
      (200, 208),
      (290, 298)
    ).foreach { case (from, to) =>
      val part=  (from, to)
      val md: Seq[(ShopItemId, Double, Int)] = region1.filter(t => t._3 >= from && t._3 <= to)
      multidia(md, part)
    }


  }

  /**
   * Creates simple submission (e.g. all mean) and adds stepwise more and more real values to see
   * what is the effect of training certain shop/items
   */
  def runSimpleWithReal(): Unit = {

    val proposedSimpleMap = proposedSimpleCompleteSeq.toMap

    def truth(shopItemId: ShopItemId): Double = {
      Util.shopItemIdToSubmissionId(shopItemId)
        .map(id => LocalTester.truthMap.getOrElse(id, 0.0))
        .getOrElse(0.0)
    }

    val proposedMeanSorted = proposedSimpleCompleteSeq
      .sortBy(t => -t._2)
      .map(t => t._1)
      .map(id => (id, truth(id)))


    def proposedManualReal(proposedManualRealMap: Map[ShopItemId, Double])(id: ShopItemId): Double = {
      proposedManualRealMap.getOrElse(id, proposedSimpleMap.getOrElse(id, 0.0))
    }

    def latestId(n: Int): Option[ShopItemId] = {
      proposedMeanSorted.map(t => t._1).take(n).lastOption
    }

    val sequences = Seq(
      ("overview", (0 to 300_000 by 500).par),
      ("small", (0 to 50).par),
      ("xsmall", (0 to 5).par),
      ("medium", (0 to 10_000 by 100).par),
      //("abyss", (30 to 40).par),
    )
    for (seq <- sequences) {
      val _data = for (n <- seq._2) yield {
        println(s"--> calculate $n")
        val pm = proposedMeanSorted.take(n).toMap
        val result = (n, latestId(n), LocalTester.test(createSubmission(proposedManualReal(pm))))
        println(s"<-- calculate $n")
        result
      }

      val data = _data.seq

      data.foreach { t =>
        val item = t._2.map(s => "%3s %7s".format(s.shopId.toString, s.itemId.toString)).getOrElse("-")
        val nstr = t._1.toString
        val estr = "%5.2f".format(t._3)
        println("%14s %10s %6s".format(item, nstr, estr))
      }


      val name = seq._1
      LineChartBuilder(s"stepwise_real_$name")
        .yLabel("mse")
        .xLabel("number of exchanged values")
        .title(s"Stepwise Real $name")
        .data(data.map(x => XY(x._1, x._3)))
        .build()

    }


  }
}
