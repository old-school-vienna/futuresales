import TrainSet.Norm
import upickle.default.{macroRW, ReadWriter => RW}
import upickle.default._

case class TrainSet(
                     rows: Seq[TrainSet.Row]
                   )

object TrainSet {

  case class Row(predictors: Seq[Double],
                 data: Double)


  case class Norm(mean: Double, stdDeviation: Double)
  object Norm{
    implicit val rw: RW[Norm] = macroRW
  }

  case class NormSet(predictors: Seq[Norm], data: Norm)
  object NormSet{
    implicit val rw: RW[NormSet] = macroRW
  }

  def normSet(trainSet: TrainSet): NormSet = {
    val cols = trainSet.rows(0).predictors.size
    val is = 0 until cols
    val predNorms: Seq[Norm] = for (i <- is) yield {
      val col = for (row <- trainSet.rows) yield row.predictors(i)
      norm(col)
    }
    val col = for (row <- trainSet.rows) yield row.data
    val dataNorm = norm(col)
    NormSet(predNorms, dataNorm)
  }

  def norm(data: Iterable[Double]): Norm = {
    val dataSeq: Seq[Double] = data.toSeq
    val mean = dataSeq.sum / dataSeq.size
    val error = dataSeq.map(v => math.pow(v - mean, 2)).sum
    val dev = math.sqrt(error / dataSeq.size)
    Norm(mean, dev)
  }

  private def convertRow(row: Row, normSet: NormSet, f: (Double, Norm) => Double): Row = {
    val np = row.predictors.zip(normSet.predictors).map{
      case (v, n) => f(v, n)
    }
    val nd = f(row.data, normSet.data)
    Row(np, nd)
  }


  def normalize(trainSet: TrainSet, normSet: NormSet): TrainSet = {
    val nrs = trainSet.rows.map(r => convertRow(r, normSet, normalize))
    TrainSet(nrs)
  }

  def deNormalize(trainSet: TrainSet, normSet: NormSet): TrainSet = {
    val nrs = trainSet.rows.map(r => convertRow(r, normSet, deNormalize))
    TrainSet(nrs)
  }

  def normalize(value: Double, norm: Norm): Double = {
    (value - norm.mean) / norm.stdDeviation
  }

  def deNormalize(value: Double, norm: Norm): Double = {
    (value * norm.stdDeviation) + norm.mean
  }

  def writeNormSet(normSet: NormSet, id: String): Unit = {
    val filename = s"data/$id.upickle"
    Util.writeString(filename, serializeNormSet(normSet))
  }

  def readNormSet(id: String): NormSet = {
    val filename = s"data/$id.upickle"
    deSerializeNormSet(Util.readString(filename))
  }

  def serializeNormSet(normSet: NormSet): String = {
    write(normSet)
  }

  def deSerializeNormSet(json: String): NormSet = {
    read[NormSet](json)
  }

}