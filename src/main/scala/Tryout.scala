import TrainSet.{Norm, NormSet}
import Util.Situation

object Tryout {

  def writeNormSet(): Unit = {
    val ns = NormSet(
      predictors = Seq(Norm(0, 1), Norm(10, 22.4)),
      data = Norm(344, 293.333))
    TrainSet.serializeNormSet(ns)
  }

  def calcValueAt33(): Unit = {
    val ids = Seq(
      ShopItemId(31, 20949), // 431.0 (ShopItemId(31,20949),List(TrainDs(33,ShopItemId(31,20949),4.9973118279570965,431.0,71)))
      ShopItemId(25, 20949), // 461.0 (ShopItemId(25,20949),List(TrainDs(33,ShopItemId(25,20949),5.0,461.0,71)))
    )

    DataProvider.readSalesTrain(situation = Situation.Full)
      .filter(t => t.month == 33)
      .filter(t => ids.contains(t.shopItemId))
      .groupBy(t => t.shopItemId)
      .toSeq
      .sortBy(t => t._1.itemId)
      .sortBy(t => t._1.shopId)
      .foreach(println(_))
  }

  def showMonths(): Unit = {
    val months = DataProvider.readSalesTrain(situation = Situation.Full)
      .groupBy(t => t.month)
      .keys
      .toSeq
      .sorted
      .mkString(", ")

    println(s"months: $months")
  }

  def readTruthForSubmissionId(): Unit = {
    val id = 56560
    val truth = LocalTester.truthMap(id)
    println(s"-- truths for $id is $truth")
  }

  def mapId(): Unit = {
    val submissionId = 56560
    val shopItemId = Util.submissionIdToShopItemId(submissionId)
    println(s"shop item id: $shopItemId")
  }

  def truthRange(): Unit = {
    val sorted = LocalTester.truthMap.values.toSeq.sorted
    val min = sorted.last
    val max = sorted.head
    println(s"truth range $min to $max")
  }
  
  def submissionLength(): Unit = {
    val testDataSize = DataProvider.readTestData().size
    val testDataDistinctSize = DataProvider.readTestData().distinct.size
    println(s"test data: $testDataSize distinct $testDataDistinctSize")
  }
  
}
