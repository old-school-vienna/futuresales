import Util._

case class TrainDs(
                    month: Int,
                    shopItemId: ShopItemId,
                    itemPrice: Double,
                    itemCnt: Double,
                    catId: Int,
                  )

case class ShopItemId(
                       shopId: Int,
                       itemId: Int,
                     )

case class TestDs(
                   id: Int,
                   shopItemId: ShopItemId,
                 )


case class SubmissionDs(
                         id: Int,
                         itemCnt: Double,
                       )

object SubmissionMean extends App {

  SubmissionJustMean.run()

  /**
   * mean over days only days with value
   * subm_mean.csv
   * 1.27689
   *
   * mean over month only days with value
   * subm_mean.csv
   * 2.94945
   *
   * mean of all ids per months including months with no submission
   * subm_mean3.csv
   * 2.04647
   */
  object SubmissionJustMean {
    def run(): Unit = {
      val trainData: Map[ShopItemId, Seq[TrainDs]] = TrainPreprocessing.read()
        .groupBy(st => st.shopItemId)
      val pm: Map[ShopItemId, Double] = proposedValuesMean(trainData)
      createSubmission(id => pm.getOrElse(id, 0.0), "data/subm_mean.csv")
    }

  }


  /**
   * 46360 23.098493626882966 List((3,13.0), (4,384.0), (5,763.0), (6,799.0), (7,820.0), (8,950.0), (9,978.0), (10,989.0), (11,1305.0), (12,899.0), (13,941.0), (14,776.0), (15,597.0), (16,602.0), (17,625.0), (18,528.0), (19,591.0), (20,639.0), (21,634.0), (22,772.0), (23,1209.0), (24,743.0), (25,180.0), (27,481.0), (28,460.0), (29,434.0), (30,482.0), (31,436.0), (32,473.0), (33,431.0))
   * 56560 17.92875 List((3,5.0), (4,83.0), (5,766.0), (6,575.0), (7,607.0), (8,764.0), (9,764.0), (10,795.0), (11,1066.0), (12,237.0), (13,192.0), (14,107.0), (15,107.0), (16,302.0), (17,771.0), (18,563.0), (19,533.0), (20,507.0), (21,491.0), (22,588.0), (23,997.0), (24,580.0), (27,476.0), (28,498.0), (29,444.0), (30,353.0), (31,323.0), (32,388.0), (33,461.0))
   *
   * TrainDs(3,31,20949,5.0,6.0,71)
   * TrainDs(4,31,20949,5.0,20.0,71)
   * TrainDs(28,12,10209,1482.33333333,18.0,28)
   */
  object AnalyseMultiItemsPerMonth {


    def run(): Unit = {
      TrainPreprocessing.read(caching = false)
        .filter(t => t.month == 3 && t.shopItemId == ShopItemId(31, 20949))
        .foreach(println(_))

    }

  }


}