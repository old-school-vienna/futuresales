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
}
