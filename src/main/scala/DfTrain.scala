

object DfTrain {

  /*
"cnt","shop_id","item_id","month_nr","cnt1","cnt_3m",
"cnt_shop1","cnt_shop_3m","cnt_item1","cnt_item_3m",
"cnt_for"

0,6,30,0,NA,NA,NA,NA,NA,NA,NA
28,6,30,1,0,NA,0,NA,0,NA,NA
10,6,30,2,28,NA,4007,NA,861,NA,NA
  */

  private def toi(s: String): Int = {
    if (s == "NA") 0
    else s.toInt
  }

  private def tod(s: String): Double = {
    if (s == "NA") 0.0
    else s.toDouble
  }


  /**
   * Prints the error for the train3 submission
   */
  def printErrorTrain3(): Unit = {
    case class Train3(
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

    def toTrain3(line: Array[String]): Train3 = {
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

    def toSubmission(in: Train3): SubmissionDs = {
      SubmissionDs(
        id = Util.shopItemIdToSubmissionId(in.shopItemId).get,
        itemCnt = in.cntFor
      )
    }

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

  /*
  df_train4.csv
    "m_run","shop_id","item_id","month_nr","cnt","cnt_for","xvars_text"
    1,6,30,0,0,NA,"cnt_item3+"
    1,6,30,1,28,NA,"cnt_item3+"
    1,6,30,2,10,NA,"cnt_item3+"
    1,6,30,3,4,0.5,"cnt_item3+"
  */
  def printErrorTrain4(): Unit = {

    case class Train4(
                       mRun: Int,
                       shopItemId: ShopItemId,
                       monthNr: Int,
                       cnt: Int,
                       cntFor: Double,
                       xvarsText: String,
                     )

    def toTrain4(line: Array[String]): Train4 = {
      Train4(
        mRun = toi(line(0)),
        shopItemId = ShopItemId(toi(line(1)), toi(line(2))),
        monthNr = toi(line(3)),
        cnt = toi(line(4)),
        cntFor = tod(line(5)),
        xvarsText = line(6).trim,
      )
    }

    def toSubmission(in: Train4): SubmissionDs = {
      SubmissionDs(
        id = Util.shopItemIdToSubmissionId(in.shopItemId).get,
        itemCnt = in.cntFor
      )
    }

    val groups = Util.readCsv("data/df_train4.csv", toTrain4)
      .groupBy(d => d.xvarsText)
      .toSeq

    val result = for ((name, values) <- groups) yield {
      val sMap: Map[Int, Double] = values
        .filter(_.monthNr == 34)
        .map(toSubmission)
        .map(s => (s.id, s.itemCnt))
        .toMap

      val submission = DataProvider.readTestData()
        .map(x => Util.shopItemIdToSubmissionId(x.shopItemId).get)
        .map(sid => SubmissionDs(sid, sMap.getOrElse(sid, 0.0)))

      val error = LocalTester.test(submission)
      (name, error)
    }

    result
      .sortBy(_._2)
      .foreach { case (name, error) =>
        println(f"--- error for df_train4 $name%50s is $error%.3f ---")
      }

  }


  /*
  df_train.csv
  "shop_id","item_id","month_nr","cnt","cnt_shop","cnt_item","cnt1","cnt2","cnt3","cnt4","cnt5","cnt6","cnt_3m","cnt_6m","cnt_shop1","cnt_shop2","cnt_shop3","cnt_shop_3m","cnt_item1","cnt_item2","cnt_item3","cnt_item_3m"
2,1495,0,0,0,0,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA
2,1495,1,0,0,0,0,NA,NA,NA,NA,NA,NA,NA,0,NA,NA,NA,0,NA,NA,NA
2,1495,2,0,0,0,0,0,NA,NA,NA,NA,NA,NA,0,0,NA,NA,0,0,NA,NA
2,1495,3,0,0,0,0,0,0,NA,NA,NA,0,NA,0,0,0,0,0,0,0,0
2,1495,4,0,0,0,0,0,0,0,NA,NA,0,NA,0,0,0,0,0,0,0,0
2,1495,5,0,0,0,0,0,0,0,0,NA,0,NA,0,0,0,0,0,0,0,0
2,1495,6,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
2,1495,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
   */

  def analyseTrain(): Unit = {

    case class Train(
                      shopItemId: ShopItemId,
                      monthNr: Int,
                      cnt: Int,
                      cntShop: Int,
                      cntItem: Int,
                      cnt1: Int,
                      cnt2: Int,
                      cnt3: Int,
                      cnt4: Int,
                      cnt5: Int,
                      cnt6: Int,
                      cnt3m: Int,
                      cnt6m: Int,
                      cntShop1: Int,
                      cntShop2: Int,
                      cntShop3: Int,
                      cntShop3m: Int,
                      cntItem1: Int,
                      cntItem2: Int,
                      cntItem3: Int,
                      cntItem3m: Int
                    )

    def toTrain(line: Array[String]): Train = {
      Train(
        shopItemId = ShopItemId(toi(line(0)), toi(line(1))),
        monthNr = toi(line(2)),
        cnt = toi(line(3)),
        cntShop = toi(line(4)),
        cntItem = toi(line(5)),
        cnt1 = toi(line(6)),
        cnt2 = toi(line(7)),
        cnt3 = toi(line(8)),
        cnt4 = toi(line(9)),
        cnt5 = toi(line(10)),
        cnt6 = toi(line(11)),
        cnt3m = toi(line(12)),
        cnt6m = toi(line(13)),
        cntShop1 = toi(line(14)),
        cntShop2 = toi(line(15)),
        cntShop3 = toi(line(16)),
        cntShop3m = toi(line(17)),
        cntItem1 = toi(line(18)),
        cntItem2 = toi(line(19)),
        cntItem3 = toi(line(20)),
        cntItem3m = toi(line(21)),
      )
    }

    def toDoubleArray(train: Train): Seq[Double] = {
      val result: Array[Double] = Array.fill(20)(0.0)
      result(0) = train.monthNr
      result(1) = train.cnt
      result(2) = train.cntShop
      result(3) = train.cntItem
      result(4) = train.cnt1
      result(5) = train.cnt2
      result(6) = train.cnt3
      result(7) = train.cnt4
      result(8) = train.cnt5
      result(9) = train.cnt6
      result(10) = train.cnt3m
      result(11) = train.cnt6m
      result(12) = train.cntShop1
      result(13) = train.cntShop2
      result(14) = train.cntShop3
      result(15) = train.cntShop3m
      result(16) = train.cntItem1
      result(17) = train.cntItem2
      result(18) = train.cntItem3
      result(19) = train.cntItem3m
      result.toSeq
    }

    def predictors(data: Iterable[Train]): Iterable[Double] = {
      data
        .filter(t => t.monthNr > 10)
        .filter(t => t.monthNr < 34)
        .flatMap(toDoubleArray)
    }

    def truth(id: ShopItemId): Double = {
      val sid = Util.shopItemIdToSubmissionId(id).get
      LocalTester.truthMap.getOrElse(sid, 0.0)
    }

    /*
    ShopItemId(31,1201)         35 
    ShopItemId(55,20956)        35 
    ShopItemId(56,1495)         35 
    ShopItemId(59,19415)        35 
    ShopItemId(57,5823)         35 
    
    Found length of every shop/item is 35
     */

    Util.readCsv("data/df_train.csv", toTrain)
      .groupBy(t => t.shopItemId)
      .map { case (id, seq) => (id, truth(id), predictors(seq)) }
      .filter(t => t._2 > 0.00001)
      .toSeq
      .sortBy(t => t._2)
      .foreach(println(_))
  }

}
