import scala.io.Source

case class TrainDs(
                    month: Int,
                    shop_id: Int,
                    item_id: Int,
                    item_price: Double,
                    item_cnt: Double,
                    cat_id: Int,
                  )

case class TestDs(
                   id: Int,
                   shopId: Int,
                   itemId: Int
                 )

case class SubmissionDs(
                         id: Int,
                         itemCnt: Double,
                       )

object SubmissionMean extends App {

  def propMapMean(trainData: Map[(Int, Int), Seq[TrainDs]]): Map[(Int, Int), Double] = {

    def meanItems(items: Iterable[TrainDs]): Double = {
      val seqItems = items.toSeq
      val mean = seqItems.map(i => i.item_cnt).sum / seqItems.size
      math.max(0.0, mean)
    }

    trainData.map(t => (t._1, meanItems(t._2)))
  }

  def toTestDs(line: Array[String]): TestDs = {
    TestDs(id = line(0).toInt,
      shopId = line(1).toInt,
      itemId = line(2).toInt
    )
  }

  def _toSubm(probMap: Map[(Int, Int), Double])(t: TestDs): SubmissionDs = {
    val pred: Double = probMap.getOrElse((t.itemId, t.shopId), 0)
    SubmissionDs(t.id, pred)
  }


  def toSubmStr(subm: SubmissionDs): String = {
    val sb = new StringBuilder()
    sb.append(subm.id)
    sb.append(",")
    sb.append("%.2f".format(subm.itemCnt))
    sb.toString()
  }

  def justMeanSubmission(): Unit = {
    val trainData: Map[(Int, Int), Seq[TrainDs]] = TrainPreprocessing.read()
      .groupBy(st => (st.item_id, st.shop_id))
    val pm: Map[(Int, Int), Double] = propMapMean(trainData)
    val tests = Util.readCsv("data/test.csv", toTestDs).map(_toSubm(pm)(_))
    val outFileName = "data/subm_mean.csv"
    Util.writeCsv(filename = outFileName,
      trains = tests,
      fMap = toSubmStr,
      header = Some("ID,item_cnt_month"))
    println(s"Wrote submissiun to $outFileName")
  }

  justMeanSubmission()
}