import Util._
import entelijan.viz.Viz.{DataRow, Diagram, MultiDiagram, XY}
import entelijan.viz.{VizCreator, VizCreators}

case class TopItem(
                    id: Int,
                    itemId: Int,
                    shopId: Int,
                    itemCountMean: Double,
                    values: Iterable[TrainDs],
                  )


object SubmissionTopManual extends App {

  val tester = LocalTester

  Submission.runTestAll()

  //Submission.runZeros(Situation.Local)
  //Submission.runAllMean(Situation.Local)
  //Submission.runAllZeros(Situation.Local)
  //Analyse.run(situation = Situation.Local)

  object Submission {

    private def proposedMeanMap(situation: Situation) = {
      val trainData = trainDataGroupedByShopItemId(situation)
      val proposedMean: Map[ShopItemId, Double] = proposedValuesMean(trainData)
      proposedMean
    }

    def proposedAllMean(proposedMeanMap: Map[ShopItemId, Double])(id: ShopItemId): Double = {
      proposedMeanMap.getOrElse(id, 0.0)
    }

    def proposedManualMean(proposedMeanMap: Map[ShopItemId, Double])(id: ShopItemId): Double = {
      proposedManual.getOrElse(id, proposedMeanMap.getOrElse(id, 0.0))
    }

    def proposedManualZero(id: ShopItemId): Double = proposedManual.getOrElse(id, 0.0)

    val proposedManual: Map[ShopItemId, Double] = Map(
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

    /**
     * all zero        31.16
     * manual and zero 17.60
     * all mean        12.89
     * manual and mean 14.70
     */
    def runTestAll(): Unit = {
      val local = Situation.Local
      val pmm = proposedMeanMap(local)
      println("created mean")
      Seq(
        ("all zero", tester.test(createSubmission(_ => 0.0))),
        ("manual and zero", tester.test(createSubmission(proposedManualZero))),
        ("all mean", tester.test(createSubmission(proposedAllMean(pmm)))),
        ("manual and mean", tester.test(createSubmission(proposedManualMean(pmm)))),
      ).map { case (t, v) => "%30s %.2f".format(t, v) }
        .foreach(println(_))
    }

    def runTestReal(): Unit = {
      val local = Situation.Local
      val pmm = proposedMeanMap(local)
      println("created mean")
      Seq(
        ("all zero", tester.test(createSubmission(_ => 0.0))),
        ("manual and zero", tester.test(createSubmission(proposedManualZero))),
        ("all mean", tester.test(createSubmission(proposedAllMean(pmm)))),
        ("manual and mean", tester.test(createSubmission(proposedManualMean(pmm)))),
      ).map { case (t, v) => "%30s %.2f".format(t, v) }
        .foreach(println(_))

    }

    def runManualInfo(): Unit = {
      def formatMan(k: ShopItemId): String = s"${k.shopId} ${k.itemId} ${proposedManual(k)}"

      proposedManual.keys.map(k => formatMan(k)).foreach(s => println(s))
    }

    def runZeros(situation: Situation): Unit = {
      createFileOrTest(situation, "zeros_and_manual", proposedManualZero)
    }

    def runAllZeros(situation: Situation): Unit = {
      createFileOrTest(situation, "all_zero", _ => 0.0)
    }


    def runManualMean(situation: Situation): Unit = {
      val pmm = proposedMeanMap(situation)
      createFileOrTest(situation, "mean_with_manual", proposedManualMean(pmm))
    }

    def runAllMean(situation: Situation): Unit = {
      val pmm = proposedMeanMap(situation)
      createFileOrTest(situation, "all_mean", proposedAllMean(pmm))
    }

    private def createFileOrTest(situation: Situation,
                                 name: String,
                                 proposed: ShopItemId => Double): Unit = {
      situation match {
        case Situation.Full => createSubmissionFile(proposed, situation, name)
        case Situation.Local =>
          val submission = createSubmission(proposed)
          val mse = tester.test(submission)
          println(f"run $name mse: $mse%.2f")
      }
    }

  }

  object Analyse {

    val itemCnt = 20
    val cols = 5
    val fontFact = 0.5

    def run(situation: Situation): Unit = {
      val trainDataMap: Map[ShopItemId, Seq[TrainDs]] = trainDataGroupedByShopItemId(situation)
      val proposedMean: Map[ShopItemId, Double] = proposedValuesMean(trainDataMap)
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
        .takeRight(itemCnt)
        .reverse
        .map(toTopItem)

      def toValueTuples(values: Iterable[TrainDs]): Seq[(Int, Double)] = {
        values.toSeq
          .map(t => (t.month, t.itemCnt))
          .sortBy(_._1)
      }

      topItems
        .foreach { t =>
          val ms = "%.2f".format(t.itemCountMean)
          println(s"    (ShopItemId(${t.shopId}, ${t.itemId}) -> 0.0), // mean:$ms")
        }

      def toMultiDiagram(topItems: Seq[TopItem]): MultiDiagram[XY] = {

        def toXy(values: Iterable[TrainDs]): Seq[XY] = {
          val vts: Map[Int, Double] = toValueTuples(values).toMap
          val maxMonth = situation match {
            case Situation.Full => 33
            case Situation.Local => 32
          }
          for (x <- 0 to maxMonth) yield {
            val y: Double = vts.getOrElse(x, 0.0)
            XY(x.toDouble, y)
          }
        }

        def toDias: Seq[Diagram[XY]] = {
          topItems.map { ti =>
            val meanStr = "%.2f".format(ti.itemCountMean)
            Diagram[XY](
              id = "dia",
              title = s"ID:${ti.id} ${ti.shopId} ${ti.itemId} $meanStr",
              //              yRange = Some(Range(Some(0.0),Some(1000.0))),
              dataRows = Seq(DataRow(data = toXy(ti.values))))
          }
        }

        MultiDiagram(id = s"topItems_$situation", columns = cols, fontFactor = fontFact, title = Some("Top Items"), diagrams = toDias)
      }

      val c: VizCreator[XY] = VizCreators.gnuplot(clazz = classOf[XY])
      c.createMultiDiagram(toMultiDiagram(topItems))

    }

  }

}


