object DfTrain3 {

  /*
"cnt","shop_id","item_id","month_nr","cnt1","cnt_3m",
"cnt_shop1","cnt_shop_3m","cnt_item1","cnt_item_3m",
"cnt_for"

0,6,30,0,NA,NA,NA,NA,NA,NA,NA
28,6,30,1,0,NA,0,NA,0,NA,NA
10,6,30,2,28,NA,4007,NA,861,NA,NA
  */

  private case class Train3(
                     cnt: Int,
                     shopItemId: ShopItemId,
                     monthNr: Int,
                     cnt1: Int,
                     cnt3m: Int,
                     cntShop1: Int,
                     cntShop3m: Int,
                     cntItem1: Int,
                     cntItem3m: Int,
                     cntFor: Double,
                   )


  private def toTrain3(line: Array[String]): Train3 = {
    def toi(s: String): Int = {
      if (s == "NA") 0
      else s.toInt
    }

    def tod(s: String): Double = {
      if (s == "NA") 0.0
      else s.toDouble
    }

    Train3(
      cnt = toi(line(0)),
      shopItemId = ShopItemId(toi(line(1)), toi(line(2))),
      monthNr = toi(line(3)),
      cnt1 = toi(line(4)),
      cnt3m = toi(line(5)),
      cntShop1 = toi(line(6)),
      cntShop3m = toi(line(7)),
      cntItem1 = toi(line(8)),
      cntItem3m = toi(line(9)),
      cntFor = tod(line(10)),
    )
  }

  private def toSubmission(in: Train3): SubmissionDs = {
    SubmissionDs(
      id = Util.shopItemIdToSubmissionId(in.shopItemId).get,
      itemCnt = in.cntFor
    )
  }

  /**
   * Prints the error for the train3 submission
   */
  def printError(): Unit = {
    val sMap: Map[Int, Double] = Util.readCsv("data/df_train3.csv", toTrain3)
      .filter(_.monthNr == 34)
      .map(toSubmission)
      .map(s => (s.id, s.itemCnt))
      .toMap

    val submission = DataProvider.readTestData()
      .map(x => Util.shopItemIdToSubmissionId(x.shopItemId).get)
      .map(sid => SubmissionDs(sid, sMap.getOrElse(sid, 0.0)))

    val mse = LocalTester.test(submission)

    println(f"--- mse for df_train3 is $mse%.3f ---")
  }

}
