import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object Util {

  sealed trait Situation

  object Situation {
    final case object Full extends Situation
    final case object Local extends Situation
  }

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

  def readTestData():Seq[TestDs] = readCsv("data/test.csv", toTestDs)

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

  def proposedValuesMean(trainData: Map[ShopItemId, Seq[TrainDs]]): Map[ShopItemId, Double] = {

    def meanItems(items: Iterable[TrainDs]): Double = {
      val seqItems = items.toSeq
      val mean = seqItems.map(i => math.max(0, i.itemCnt)).sum / 34
      mean
    }

    trainData.map(t => (t._1, meanItems(t._2)))
  }

  def toTestDs(line: Array[String]): TestDs = {
    TestDs(id = line(0).toInt,
      shopItemId = ShopItemId(line(1).toInt, line(2).toInt),
    )
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

  def createSubmissionFile(fProposedValues: ShopItemId => Double, situation: Situation, name: String):Unit = {
    val tests = createSubmission(fProposedValues)
    val outFileName = situation match {
      case Situation.Full => s"data/submission_$name.csv"
      case Situation.Local =>s"data/submission_local_$name.csv"
    }
    Util.writeCsv(filename = outFileName,
      trains = tests,
      fMap = toSubmStr,
      header = Some("ID,item_cnt_month"))
    println(s"Wrote submission to $outFileName")
  }

  def createSubmission(fProposedValues: ShopItemId => Double): Seq[SubmissionDs] = {
    readTestData()
      .map(toSubm(fProposedValues)(_))
  }
}
