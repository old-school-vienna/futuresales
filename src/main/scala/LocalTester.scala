import Util.{Situation, shopItemIdToSubmissionId}

object LocalTester {

  def calcTruth(shopItemId: ShopItemId, values: Seq[TrainDs]): Option[SubmissionDs] = {
    val last = values.filter(t => t.month == 33)
    val n = last.size
    val sum: Double = last match {
      case Nil => 0.0
      case seq => seq.map(t => t.itemCnt).sum
    }
    val mean = n match {
      case 0 => 0.0
      case n1 => sum / n1
    }
    shopItemIdToSubmissionId(shopItemId = shopItemId).map(i => SubmissionDs(i, mean))
  }

  def initTruth: Map[Int, Double] = {
    DataProvider.readSalesTrain(situation = Situation.Full)
      .groupBy(t => t.shopItemId)
      .flatMap { case (id, values) => calcTruth(id, values) }
      .map(s => (s.id, s.itemCnt))
      .toMap
  }

  val truthMap: Map[Int, Double] = initTruth

  def test(data: Iterable[SubmissionDs]): Double = {

    def squaredError(ds: SubmissionDs): Double = {
      val t = truthMap.getOrElse(ds.id, 0.0)
      val v = ds.itemCnt
      math.pow(t - v, 2)
    }

    val n = data.size
    data.map(d => squaredError(d)).sum / n
  }

}
