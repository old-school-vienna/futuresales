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

  Submission.runAllZeros()

  //Analyse.

  //noinspection DuplicatedCode
  object Submission {

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


    def runManualInfo(): Unit = {
      def formatMan(k: ShopItemId): String = {
        s"${k.shopId} ${k.itemId} ${proposedManual(k)}"
      }

      proposedManual.keys.map(k => formatMan(k)).foreach(s => println(s))
    }

    def runZeros(): Unit = {
      def proposed(id: ShopItemId): Double = {
        if (proposedManual.isDefinedAt(id)) {
          proposedManual.getOrElse(id, 0.0)
        }
        proposedManual.getOrElse(id, 0.0)
      }

      createSubmission(proposed, "data/subm_zeros_top_manual.csv")
    }

    def runAllZeros(): Unit = {
      def proposed(id: ShopItemId): Double = {
        0.0
      }

      createSubmission(proposed, "data/subm_all_zeros_ww.csv")
    }

    def runMean(): Unit = {
      val trainData: Map[ShopItemId, Seq[TrainDs]] =
        TrainPreprocessing.read().groupBy(st => st.shopItemId)
      val pm: Map[ShopItemId, Double] = proposedValuesMean(trainData)

      def proposed(id: ShopItemId): Double = {
        proposedManual.getOrElse(id, pm.getOrElse(id, 0.0))
      }

      createSubmission(proposed, "data/subm_top_manual.csv")
    }

  }

  object Analyse {

    val itemCnt = 20
    val cols = 5
    val fontFact = 0.5

    def run(): Unit = {
      val trainDataMap: Map[ShopItemId, Seq[TrainDs]] = TrainPreprocessing.read()
        .groupBy(st => st.shopItemId)
      val pm: Map[ShopItemId, Double] = proposedValuesMean(trainDataMap)
      val testData: Seq[TestDs] = readTestData()

      val idMap: Map[Int, ShopItemId] = testData.map(t => (t.id, t.shopItemId)).toMap

      def toTopItem(submissionDs: SubmissionDs): TopItem = {
        val ids: ShopItemId = idMap(submissionDs.id)
        val vals = trainDataMap(ids)
        TopItem(id = submissionDs.id,
          itemId = ids.itemId,
          shopId = ids.shopId,
          itemCountMean = submissionDs.itemCnt,
          values = vals)
      }

      val topItems: Seq[TopItem] = testData
        .map(toSubm(id => pm.getOrElse(id, 0.0))(_))
        .sortBy(t => t.itemCnt)
        .takeRight(itemCnt)
        .reverse
        .map(toTopItem)

      def toValueTupels(values: Iterable[TrainDs]): Seq[(Int, Double)] = {
        values.toSeq
          .map(t => (t.month, t.itemCnt))
          .sortBy(_._1)
      }

      topItems
        .foreach { t =>
          val mstr = "%.2f".format(t.itemCountMean)
          println(s"    (ShopItemId(${t.shopId}, ${t.itemId}) -> 0.0), // mean:$mstr")
        }

      def toMultiDiagram(topItems: Seq[TopItem]): MultiDiagram[XY] = {

        def toXy(vals: Iterable[TrainDs]): Seq[XY] = {
          val vts: Map[Int, Double] = toValueTupels(vals).toMap
          for (x <- 0 to 33) yield {
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

        MultiDiagram(id = "topItems", columns = cols, fontFactor = fontFact, title = Some("Top Items"), diagrams = toDias)
      }

      val c: VizCreator[XY] = VizCreators.gnuplot(clazz = classOf[XY])
      c.createMultiDiagram(toMultiDiagram(topItems))

    }

  }

}


