import Util._
import entelijan.viz.Viz._
import entelijan.viz.VizCreators
import entelijan.vizb.LineChartBuilder


case class TopItem(
                    id: Int,
                    itemId: Int,
                    shopId: Int,
                    itemCountMean: Double,
                    values: Iterable[TrainDs],
                  )


object SubmissionTopManual {

  private val proposedManualSeq: Seq[(ShopItemId, Double)] = Seq(
    ShopItemId(31, 20949) -> 410, // mean:586.29
    ShopItemId(25, 20949) -> 410, // mean:421.85
    ShopItemId(28, 20949) -> 200, // mean:395.82
    ShopItemId(42, 20949) -> 480, // mean:309.24
    ShopItemId(12, 11373) -> 400, // mean:193.03
    ShopItemId(12, 11370) -> 200, // mean:181.26
    ShopItemId(57, 20949) -> 100, // mean:179.88
    ShopItemId(47, 20949) -> 150, // mean:133.24
    ShopItemId(22, 20949) -> 50, // mean:128.88
    ShopItemId(21, 20949) -> 150, // mean:127.50
    ShopItemId(46, 20949) -> 100, // mean:114.44
    ShopItemId(26, 20949) -> 100, // mean:106.59
    ShopItemId(6, 20949) -> 50, // mean:104.44
    ShopItemId(53, 20949) -> 70, // mean:104.12
    ShopItemId(56, 20949) -> 80, // mean:101.15
    ShopItemId(35, 20949) -> 60, // mean:101.00
    ShopItemId(16, 20949) -> 50, // mean:99.85
    ShopItemId(7, 20949) -> 50, // mean:97.68
    ShopItemId(14, 20949) -> 70, // mean:94.68
    ShopItemId(58, 20949) -> 70, // mean:86.12
  )

  private val proposedManualMap: Map[ShopItemId, Double] = proposedManualSeq.toMap

  private def proposedMeanMap(situation: Situation) = {
    val trainData = trainDataGroupedByShopItemId(situation)
    val proposedMean: Map[ShopItemId, Double] = proposedValuesMean(trainData, situation, 0.8)
    proposedMean
  }

  private def proposedAllMean(proposedMeanMap: Map[ShopItemId, Double], factor: Double = 1.0)(id: ShopItemId): Double = {
    proposedMeanMap.getOrElse(id, 0.0) * factor
  }

  private def proposedManualMean(proposedMeanMap: Map[ShopItemId, Double])(id: ShopItemId): Double = {
    proposedManualMap.getOrElse(id, proposedMeanMap.getOrElse(id, 0.0))
  }

  private def proposedManualZero(id: ShopItemId): Double = proposedManualMap.getOrElse(id, 0.0)

  /**
   * Show that testing a submission with all real values leads to error 0.0 
   */
  def printErrorAllReal(): Unit = {
    val truth = LocalTester.truthMap

    def proposeReal(shopItemId: ShopItemId): Double = {
      val submissionId = Util.shopItemIdToSubmissionId(shopItemId)
      submissionId.map(sid => truth.getOrElse(sid, 0.0)).getOrElse(0.0)
    }

    val submission = createSubmission(proposeReal)
    val result = "%.4f".format(LocalTester.test(submission))
    println(s"All real should lead to 0.0. is:$result")
  }

  /**
   * Plot the error for mean submissions with factor
   */
  def plotErrorForMeanWithFactor(): Unit = {
    val pmm = proposedMeanMap(Situation.Local)
    val mse = Util.fromToStep(0.5, 1.5, 0.01).map { f =>
      val fs = "%5.2f".format(f)
      (s"mean with factor $fs", f, LocalTester.test(createSubmission(proposedAllMean(pmm, f))))
    }
    mse.map { case (t, _, v) => "%30s %5.3f".format(t, v) }.foreach(println(_))
    LineChartBuilder("mean_with_factor")
      .title("mean with factor")
      .xySeq(mse.map { case (_, x, y) => XY(x, y) })
      .create()
  }

  def createKaggleSubmissionManualZero(situation: Situation): Unit = {
    createFileOrTest(situation, "zeros_and_manual", proposedManualZero)
  }

  def createKaggleSubmissionAllZeros(situation: Situation): Unit = {
    createFileOrTest(situation, "all_zero", _ => 0.0)
  }


  def createKaggleSubmissionManualMean(situation: Situation): Unit = {
    val pmm = proposedMeanMap(situation)
    createFileOrTest(situation, "mean_with_manual", proposedManualMean(pmm))
  }

  def createKaggleSubmissionAllMean(situation: Situation): Unit = {
    val pmm = proposedMeanMap(situation)
    createFileOrTest(situation, "all_mean", proposedAllMean(pmm))
  }
  
  def printEffectOfManual(): Unit = {
    val situation = Situation.Local
    val pmm = proposedMeanMap(situation)
    createFileOrTest(situation, "all_mean", proposedAllMean(pmm))
    createFileOrTest(situation, "mean_with_manual", proposedManualMean(pmm))
    createFileOrTest(situation, "all_zero", _ => 0.0)
    createFileOrTest(situation, "zeros_with_manual", proposedManualZero)
  }

  def plotTopMeanFull(): Unit = {
    val situation = Situation.Full
    val trainDataMap: Map[ShopItemId, Seq[TrainDs]] = trainDataGroupedByShopItemId(situation)
    val proposedMean: Map[ShopItemId, Double] = proposedValuesMean(trainDataMap, situation)
    val testData: Seq[TestDs] = DataProvider.readTestData()

    val idMap: Map[Int, ShopItemId] = testData.map(t => (t.id, t.shopItemId)).toMap

    def toTopItem(submissionDs: SubmissionDs): TopItem = {
      val ids: ShopItemId = idMap(submissionDs.id)
      val values = trainDataMap(ids)
      TopItem(id = submissionDs.id,
        itemId = ids.itemId,
        shopId = ids.shopId,
        itemCountMean = submissionDs.itemCnt,
        values = values)
    }

    val topItems: Seq[TopItem] = testData
      .map(toSubm(id => proposedMean.getOrElse(id, 0.0))(_))
      .sortBy(t => t.itemCnt)
      .takeRight(20)
      .reverse
      .map(toTopItem)


    topItems
      .foreach { t =>
        val ms = "%.2f".format(t.itemCountMean)
        println(s"    (ShopItemId(${t.shopId}, ${t.itemId}) -> 0.0), // mean:$ms")
      }

    def toMultiDiagram(topItems: Seq[TopItem]): MultiDiagram[XY] = {

      def toValueTuples(values: Iterable[TrainDs]): Seq[(Int, Double)] = {
        values.toSeq
          .map(t => (t.month, t.itemCnt))
          .sortBy(_._1)
      }

      def toXy(values: Iterable[TrainDs]): Seq[XY] = {
        for (x <- 0 to 32) yield {
          val y: Double = toValueTuples(values).toMap.getOrElse(x, 0.0)
          XY(x.toDouble, y)
        }
      }

      def toDias: Seq[Diagram[XY]] = {
        topItems.map { ti =>
          val meanStr = "%.2f".format(ti.itemCountMean)
          Diagram[XY](
            id = "dia",
            title = s"ID:${ti.id} ${ti.shopId} ${ti.itemId} $meanStr",
            dataRows = Seq(DataRow(data = toXy(ti.values))))
        }
      }

      MultiDiagram(id = s"topItems_$situation", columns = 5, fontFactor = 0.5, title = Some("Top Items"), diagrams = toDias)
    }

    VizCreators.gnuplot(clazz = classOf[XY]).createMultiDiagram(toMultiDiagram(topItems))

  }

  private def createFileOrTest(situation: Situation,
                               name: String,
                               proposed: ShopItemId => Double): Unit = {
    situation match {
      case Situation.Full => createSubmissionFile(proposed, situation, name)
      case Situation.Local =>
        val submission = createSubmission(proposed)
        val mse = LocalTester.test(submission)
        println(f"run $name mse: $mse%.2f")
    }
  }

}


