import java.io.{BufferedWriter, FileWriter}
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}

import entelijan.viz.Viz.XY

import scala.io.{Codec, Source}

object Util {

  private lazy val _shopItemIdMap = DataProvider.readTestData().map(d => (d.shopItemId, d.id)).toMap

  private lazy val _subMissionIdMap = DataProvider.readTestData().map(d => (d.id, d.shopItemId)).toMap

  sealed trait Situation

  object Situation {

    final case object Full extends Situation

    final case object Local extends Situation

  }

  val errorItemsSituationLocal = Seq(
    ShopItemId(12, 11373),
    ShopItemId(25, 10201),
    ShopItemId(42, 10201),
    ShopItemId(42, 10202),
    ShopItemId(25, 7224),
    ShopItemId(25, 10202),
    ShopItemId(12, 20949),
  )

  def toi(s: String): Int = {
    if (s == "NA") 0
    else s.toInt
  }

  def tod(s: String): Double = {
    if (s == "NA") 0.0
    else s.toDouble
  }

  def trainDataGroupedByShopItemId(situation: Situation): Map[ShopItemId, Seq[TrainDs]] = {
    DataProvider.readSalesTrain(situation).groupBy(st => st.shopItemId)
  }

  def shopItemIdToSubmissionId(shopItemId: ShopItemId): Option[Int] = _shopItemIdMap.get(shopItemId)

  def submissionIdToShopItemId(submissionId: Int): ShopItemId = _subMissionIdMap(submissionId)

  def readCsv[T](
                  filename: Path,
                  fMap: Array[String] => T,
                  headerLines: Int = 1,
                  separator: String = ","): Seq[T] = {

    val src = Source.fromFile(filename.toFile)
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

  def writeCsv[T](filename: Path,
                  trains: Iterable[T],
                  fMap: T => String,
                  header: Option[String] = None): Unit = {
    val bw = new BufferedWriter(new FileWriter(filename.toFile, Charset.forName("UTF-8")))
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

  def readString(filename: Path): String = {
    val src = Source.fromFile(filename.toFile)(Codec.UTF8)
    try {
      src.getLines().mkString("")
    } finally {
      src.close()
    }
  }

  def writeString(filename: Path, string: String): Unit = {
    val bw = new BufferedWriter(new FileWriter(filename.toFile))
    try {
      bw.write(string)
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

  def toSubmission(fProposedValues: ShopItemId => Double)(t: TestDs): SubmissionDs = {
    val pred: Double = fProposedValues(t.shopItemId)
    SubmissionDs(t.id, pred)
  }


  def toSubmissionString(submission: SubmissionDs): String = {
    val sb = new StringBuilder()
    sb.append(submission.id)
    sb.append(",")
    sb.append("%.2f".format(submission.itemCnt))
    sb.toString()
  }

  def createSubmissionFile(fProposedValues: ShopItemId => Double, situation: Situation, name: String): Unit = {
    val tests = createSubmission(fProposedValues)
    val outFileName = situation match {
      case Situation.Full => Util.outputDirectory.resolve(s"submission_$name.csv")
      case Situation.Local => Util.outputDirectory.resolve(s"submission_local_$name.csv")
    }
    Util.writeCsv(filename = outFileName,
      trains = tests,
      fMap = toSubmissionString,
      header = Some("ID,item_cnt_month"))
    println(s"Wrote submission to $outFileName")
  }

  def createSubmission(fProposedValues: ShopItemId => Double): Seq[SubmissionDs] = {
    DataProvider.readTestData()
      .map(toSubmission(fProposedValues)(_))
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

  def inputDirectory: Path = directory("INPUT_DIRECTORY")

  def outputDirectory: Path = directory("OUTPUT_DIRECTORY")

  private def directory(envName: String): Path = {
    val envValue = System.getenv(envName)
    val directoryPath =
      if (envValue == null) Paths.get("data")
      else Paths.get(envValue)
    if (!Files.exists(directoryPath)) {
      Files.createDirectories(directoryPath)
    }
    directoryPath
  }

}
