import Util.Situation

object LocalTester {

  val shopItemIdMap: Map[ShopItemId, Int] = DataProvider.readTestData().map(td => (td.shopItemId, td.id)).toMap

  def calcTruth(id: ShopItemId, values: Seq[TrainDs]): Option[SubmissionDs] = {
    val last = values.filter(t => t.month == 23)
    val n = last.size
    val sum: Double = last match {
      case Nil => 0.0
      case seq => seq.map(t => t.itemCnt).sum
    }
    val mean = n match {
      case 0 => 0.0
      case n1 => sum / n1
    }
    shopItemIdMap.get(id).map(i => SubmissionDs(i, mean))
  }

  def initTruth: Map[Int, Double] = {
    DataProvider.readSalesTrain(situation = Situation.Local)
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
