import Util._
import entelijan.viz.Viz._
import entelijan.viz.VizCreators

import scala.collection.parallel.CollectionConverters._

case class TopItem(
                    id: Int,
                    itemId: Int,
                    shopId: Int,
                    itemCountMean: Double,
                    values: Iterable[TrainDs],
                  )


object SubmissionTopManual extends App {

  Submission.runMeanWithReal()

  object Submission {

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
      val proposedMean: Map[ShopItemId, Double] = proposedValuesMean(trainData, situation)
      proposedMean
    }

    private def proposedAllMean(proposedMeanMap: Map[ShopItemId, Double])(id: ShopItemId): Double = {
      proposedMeanMap.getOrElse(id, 0.0)
    }

    private def proposedManualMean(proposedMeanMap: Map[ShopItemId, Double])(id: ShopItemId): Double = {
      proposedManualMap.getOrElse(id, proposedMeanMap.getOrElse(id, 0.0))
    }

    private def proposedManualZero(id: ShopItemId): Double = proposedManualMap.getOrElse(id, 0.0)

    def runAllReal(): Unit = {
      val truth = LocalTester.truthMap

      def proposeReal(shopItemId: ShopItemId): Double = {
        val submissionId = Util.shopItemIdToSubmissionId(shopItemId)
        submissionId.map(sid => truth.getOrElse(sid, 0.0)).getOrElse(0.0)
      }

      val submission = createSubmission(proposeReal)
      val result = "%.4f".format(LocalTester.test(submission))
      println(s"All real shoul lead to 0.0. $result")
    }

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
        ("all zero", LocalTester.test(createSubmission(_ => 0.0))),
        ("manual and zero", LocalTester.test(createSubmission(proposedManualZero))),
        ("all mean", LocalTester.test(createSubmission(proposedAllMean(pmm)))),
        ("manual and mean", LocalTester.test(createSubmission(proposedManualMean(pmm)))),
      ).map { case (t, v) => "%30s %.2f".format(t, v) }
        .foreach(println(_))
    }

    /**
     * Creates submissions for all mean and adds stepwise more and more real values to see
     * what is the effect of training certain shop/items
     */
    def runMeanWithReal(): Unit = {
      val trainDataMap: Map[ShopItemId, Seq[TrainDs]] = trainDataGroupedByShopItemId(Situation.Local)
      val proposedMeanMap: Map[ShopItemId, Double] = proposedValuesMean(trainDataMap, Situation.Local)

      def truth(shopItemId: ShopItemId): Double = {
        Util.shopItemIdToSubmissionId(shopItemId)
          .map(id => LocalTester.truthMap.getOrElse(id, 0.0))
          .getOrElse(0.0)
      }

      val proposedMeanSorted = proposedMeanMap
        .toList
        .sortBy(t => -t._2)
        .map(t => t._1)
        .map(id => (id, truth(id)))


      def proposedManualReal(proposedManualRealMap: Map[ShopItemId, Double])(id: ShopItemId): Double = {
        proposedManualRealMap.getOrElse(id, proposedMeanMap.getOrElse(id, 0.0))
      }

      def latestId(n: Int): Option[ShopItemId] = {
        proposedMeanSorted.map(t => t._1).take(n).lastOption
      }

      val sequences = Seq(
        //("overview", Seq(0, 100, 500, 1000, 5000, 10_000, 50_000, 100_000, 200_000, 300_000).par),
        //("medium", (0 to 200).par),
        //("small", (0 to 50).par),
        ("test", Seq(100_000, 200_000, 250_000, 300_000).par),
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


        val dataRow = DataRow(data = data.map(x => XY(x._1, x._3)))
        val dia = Diagram[XY](id = s"use_real_${seq._1}",
          title = s"Exchange mean by Real Values (${seq._1})",
          yRange = Some(Range(Some(0), None)),
          dataRows = Seq(dataRow))

        VizCreators.gnuplot(clazz = classOf[XY]).createDiagram(dia = dia)
      }


    }


    def runManualInfo(): Unit = {
      def formatMan(k: ShopItemId): String = s"${k.shopId} ${k.itemId} ${proposedManualMap(k)}"

      proposedManualMap.keys.map(k => formatMan(k)).foreach(s => println(s))
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
          val mse = LocalTester.test(submission)
          println(f"run $name mse: $mse%.2f")
      }
    }

  }

  object Analyse {

    def runTopMean(situation: Situation): Unit = {
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

        MultiDiagram(id = s"topItems_$situation", columns = 5, fontFactor = 0.5, title = Some("Top Items"), diagrams = toDias)
      }

      VizCreators.gnuplot(clazz = classOf[XY]).createMultiDiagram(toMultiDiagram(topItems))

    }


    def runBadAtMean(): Unit = {
      val items = Seq(
        ShopItemId(31, 20949),
        ShopItemId(42, 20949),
        ShopItemId(22, 20949),
        ShopItemId(12, 11373),
        ShopItemId(21, 20949),
        ShopItemId(30, 20949),
      )


      def _truth(ti: ShopItemId): Double = {
        Util.shopItemIdToSubmissionId(ti)
          .map(id => LocalTester.truthMap.getOrElse(id, 0.0))
          .getOrElse(0.0)
      }

      def toMultiDiagram(topItems: Seq[ShopItemId]): MultiDiagram[XY] = {

        def toXy(item: ShopItemId): Seq[XY] = {

          def meanOfSales(data: Seq[TrainDs]): Double = {
            val sum = data.map(d => d.itemCnt).sum
            sum / data.size
          }

          val monthMap: Map[Int, Double] = DataProvider.readSalesTrain(Situation.Local)
            .filter(x => x.shopItemId == item)
            .groupBy(x => x.month)
            .map(t => (t._1, meanOfSales(t._2)))

          for (m <- 0 to 32) yield {
            val value = monthMap.getOrElse(m, 0.0)
            XY(m, value)
          }

        }

        def toDias: Seq[Diagram[XY]] = {
          topItems.map { ti =>
            val truth = "%.2f".format(_truth(ti))
            Diagram[XY](
              id = "dia",
              title = s"shop item: ${ti.shopId} ${ti.itemId} truth; $truth",
              yRange = Some(Range(Some(0.0), Some(1000.0))),
              dataRows = Seq(DataRow(data = toXy(ti))))
          }
        }

        MultiDiagram(id = s"topItems_real", columns = 3, fontFactor = 0.7, title = Some("Top Items real"), diagrams = toDias)
      }

      VizCreators.gnuplot(clazz = classOf[XY]).createMultiDiagram(toMultiDiagram(items))
    }
  }

}


