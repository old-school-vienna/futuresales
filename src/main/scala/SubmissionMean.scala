import Util._

object SubmissionMean {

  /**
   * Create a kaggle submission. All proposition mean of months
   * 
   * mean over days only days with value
   * submission_mean.csv
   * 1.27689
   *
   * mean over month only days with value
   * submission_mean.csv
   * 2.94945
   *
   * mean of all ids per months including months with no submission
   * subm_mean3.csv
   * 2.04647
   */
  def submissionFileJustMean(): Unit = {
    val situation = Situation.Full
    val trainData: Map[ShopItemId, Seq[TrainDs]] = DataProvider.readSalesTrain(situation)
      .groupBy(st => st.shopItemId)
    val pm: Map[ShopItemId, Double] = proposedValuesMean(trainData, situation, 0.8)
    createSubmissionFile(id => pm.getOrElse(id, 0.0), situation, "mean")
  }


}