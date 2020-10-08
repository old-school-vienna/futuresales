import Util.{tod, toi}

object DfTrain4 {

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

    val filename = Util.outputDirectory.resolve("df_train4.csv")
    val groups = Util.readCsv(filename, toTrain4)
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
}
