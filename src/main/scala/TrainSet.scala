case class TrainSet(
  rows: Seq[TrainSet.Row]
                       )

object TrainSet {

  case class Row(predictors: Seq[Double],
                 data: Double)

  case class NormSet(predictors: Seq[Norm], data:Norm)

  case class Norm(
                   mean: Double,
                   stdDeviation: Double,
                 )

  def norm(data: Iterable[Double]): Norm = {
    val dataSeq: Seq[Double] = data.toSeq
    val mean = dataSeq.sum / dataSeq.size
    val error = dataSeq.map(v => math.pow(v - mean, 2)).sum
    val dev = math.sqrt(error / dataSeq.size)
    Norm(mean, dev)
  }

  def normalize(value: Double, norm: Norm): Double = {
    (value - norm.mean) / norm.stdDeviation
  }



}