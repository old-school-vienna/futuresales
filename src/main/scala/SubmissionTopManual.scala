import entelijan.viz.Viz.{DataRow, Diagram, MultiDiagram, Range, XY}
import entelijan.viz.{VizCreator, VizCreators}
import Util._

case class TopItem(
                    id: Int,
                    itemId: Int,
                    shopId: Int,
                    itemCountMean: Double,
                    values: Iterable[TrainDs],
                  )


object SubmissionTopManual extends App {

  Analyse.run()

  object Analyse {

    val itemCnt = 36
    val cols = 6
    val fontFact = 0.5

    def run(): Unit = {
      val trainDataMap: Map[ShopItemId, Seq[TrainDs]] = TrainPreprocessing.read()
        .groupBy(st => st.shopItemId)
      val pm: Map[ShopItemId, Double] = propMapMean(trainDataMap)
      val tests: Seq[TestDs] = Util.readCsv("data/test.csv", toTestDs)

      val idMap: Map[Int, ShopItemId] = tests.map(t => (t.id, t.shopItemId)).toMap

      def toTopItem(submissionDs: SubmissionDs): TopItem = {
        val ids: ShopItemId = idMap(submissionDs.id)
        val vals = trainDataMap(ids)
        TopItem(id = submissionDs.id,
          itemId = ids.itemId,
          shopId = ids.shopId,
          itemCountMean = submissionDs.itemCnt,
          values = vals)
      }

      val topItems: Seq[TopItem] = tests
        .map(toSubm(pm)(_))
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
        .map(t => s"${t.id} ${t.itemCountMean} ${toValueTupels(t.values)}")
        .foreach(println(_))

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


