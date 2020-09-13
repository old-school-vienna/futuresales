import entelijan.viz.Viz.{DataRow, Diagram, MultiDiagram, XY}
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

    def run(): Unit = {
      val trainDataMap: Map[(Int, Int), Seq[TrainDs]] = TrainPreprocessing.read()
        .groupBy(st => (st.item_id, st.shop_id))
      val pm: Map[(Int, Int), Double] = propMapMean(trainDataMap)
      val tests: Seq[TestDs] = Util.readCsv("data/test.csv", toTestDs)

      val idMap: Map[Int, (Int, Int)] = tests.map(t => (t.id, (t.shopId, t.itemId))).toMap

      def toTopItem(submissionDs: SubmissionDs): TopItem = {
        val ids = idMap(submissionDs.id)
        val vals = trainDataMap((ids._2, ids._1))
        TopItem(id = submissionDs.id,
          itemId = ids._2,
          shopId = ids._1,
          itemCountMean = submissionDs.itemCnt,
          values = vals)
      }

      val topItems: Seq[TopItem] = tests
        .map(toSubm(pm)(_))
        .sortBy(t => t.itemCnt)
        .takeRight(9)
        .reverse
        .map(toTopItem)

      def toValueTupels(values: Iterable[TrainDs]): Seq[(Int, Double)] = {
        def tt(month: Int, ds: Iterable[TrainDs]): (Int, Double) = {
          val sum = ds.map(_.item_cnt).sum
          (month, sum)
        }

        values.toSeq
          .groupBy(_.month)
          .map(t => tt(t._1, t._2))
          .toSeq
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
              title = s"ID:${ti.id} shop:${ti.shopId} item:${ti.itemId} mean:$meanStr",
              dataRows = Seq(DataRow(data = toXy(ti.values))))
          }
        }

        MultiDiagram(id = "topItems", columns = 3, fontFactor = 0.7, title = Some("Top Items"), diagrams = toDias)
      }

      val c: VizCreator[XY] = VizCreators.gnuplot(clazz = classOf[XY])
      c.createMultiDiagram(toMultiDiagram(topItems))

    }

  }

}


