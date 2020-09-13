import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

object Util {

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
  def propMapMean(trainData: Map[(Int, Int), Seq[TrainDs]]): Map[(Int, Int), Double] = {

    def meanItems(items: Iterable[TrainDs]): Double = {
      val seqItems = items.toSeq
      val mean = seqItems.map(i => math.max(0, i.item_cnt)).sum / 34
      mean
    }

    trainData.map(t => (t._1, meanItems(t._2)))
  }

  def toTestDs(line: Array[String]): TestDs = {
    TestDs(id = line(0).toInt,
      shopId = line(1).toInt,
      itemId = line(2).toInt
    )
  }

  def toSubm(probMap: Map[(Int, Int), Double])(t: TestDs): SubmissionDs = {
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


}
