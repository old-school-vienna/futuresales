import java.io.{BufferedWriter, File, FileWriter}

import entelijan.viz.Viz.XY

import scala.io.Source

object Util {

  case class Norm(
                   mean: Double,
                   stdDeviation: Double,
                 )

  def norm(data: Iterable[Double]): Norm = {
    val d1: Seq[Double] = data.toSeq
    val mean = d1.sum / d1.size
    val sum = d1.map(v => math.pow(v - mean, 2)).sum
    val dev = math.sqrt(sum / d1.size)
    Norm(mean, dev)
  }

  def normalize(value: Double, norm: Norm): Double = {
    ???
  }

  private lazy val _shopItemIdMap = DataProvider.readTestData().map(d => (d.shopItemId, d.id)).toMap

  private lazy val _subMissionIdMap = DataProvider.readTestData().map(d => (d.id, d.shopItemId)).toMap

  sealed trait Situation

  object Situation {

    final case object Full extends Situation

    final case object Local extends Situation

  }

  def trainDataGroupedByShopItemId(situation: Situation): Map[ShopItemId, Seq[TrainDs]] = {
    DataProvider.readSalesTrain(situation).groupBy(st => st.shopItemId)
  }

  def shopItemIdToSubmissionId(shopItemId: ShopItemId): Option[Int] = _shopItemIdMap.get(shopItemId)

  def submissionIdToShopItemId(submissionId: Int): ShopItemId = _subMissionIdMap(submissionId)

  def readCsv[T](
                  filename: String,
                  fMap: Array[String] => T,
                  headerLines: Int = 1,
                  separator: String = ","): Seq[T] = {

    val src = Source.fromFile(filename)
    try {
      src.getLines()
        .drop(headerLines)
        .map(_.split(separator))
        .map(fMap)
        .toSeq
    } finally {
      src.close()
    }
  }

  def writeCsv[T](filename: String,
                  trains: Iterable[T],
                  fMap: T => String,
                  header: Option[String] = None): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    try {
      header.foreach(h => {
        bw.write(h)
        bw.write("\n")
      })
      trains
        .map(fMap)
        .foreach(line => {
          bw.write(line)
          bw.write("\n")
        })
    } finally {
      bw.close()
    }
  }

  def proposedValuesMean(trainData: Map[ShopItemId, Seq[TrainDs]], situation: Situation, factor: Double = 1.0): Map[ShopItemId, Double] = {

    val length = situation match {
      case Situation.Full => 34
      case Situation.Local => 33
    }

    def meanItems(items: Iterable[TrainDs]): Double = {
      val seqItems = items.toSeq
      (seqItems.map(i => i.itemCnt).sum / length) * factor
    }

    trainData.map(t => (t._1, meanItems(t._2)))
  }

  def toSubm(fProposedValues: ShopItemId => Double)(t: TestDs): SubmissionDs = {
    val pred: Double = fProposedValues(t.shopItemId)
    SubmissionDs(t.id, pred)
  }


  def toSubmStr(subm: SubmissionDs): String = {
    val sb = new StringBuilder()
    sb.append(subm.id)
    sb.append(",")
    sb.append("%.2f".format(subm.itemCnt))
    sb.toString()
  }

  def createSubmissionFile(fProposedValues: ShopItemId => Double, situation: Situation, name: String): Unit = {
    val tests = createSubmission(fProposedValues)
    val outFileName = situation match {
      case Situation.Full => s"data/submission_$name.csv"
      case Situation.Local => s"data/submission_local_$name.csv"
    }
    Util.writeCsv(filename = outFileName,
      trains = tests,
      fMap = toSubmStr,
      header = Some("ID,item_cnt_month"))
    println(s"Wrote submission to $outFileName")
  }

  def createSubmission(fProposedValues: ShopItemId => Double): Seq[SubmissionDs] = {
    DataProvider.readTestData()
      .map(toSubm(fProposedValues)(_))
  }

  def toXy(values: Iterable[TrainDs], situation: Situation): Seq[XY] = {

    def toValueTuples(values: Iterable[TrainDs]): Seq[(Int, Double)] = {
      values.toSeq
        .map(t => (t.month, t.itemCnt))
        .sortBy(_._1)
    }

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

  def fromToStep(from: Double, to: Double, step: Double): Seq[Double] = {
    (BigDecimal(from) to BigDecimal(to) by BigDecimal(step)).map(_.toDouble)
  }

}
