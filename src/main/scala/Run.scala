object Run {

  private case class Action(
                             id: String,
                             desc: String,
                             action: () => Unit
                           )

  private val actions = Seq(
    Action("pe3", "Prints the error for the train3 submission", () => DfTrain.printErrorTrain3()),
    Action("pe4", "Prints the error for the train4 submission", () => DfTrain.printErrorTrain4()),
    Action("at", "Some analyse on df_train", () => DfTrain.analyseTrain()),
    Action("pear", "Show that testing a submission with all real values leads to error zero", () => SubmissionTopManual.printErrorAllReal()),
    Action("plef", "Plot the error for mean submissions with factor", () => SubmissionTopManual.plotErrorForMeanWithFactor()),
    Action("sam", "Create a kaggle submissin. All proposition mean of months", () => SubmissionMean.submissionFileJustMean()),
    Action("pmt", "Plots the monthly sales for the top shop/item(s)", () => SubmissionTopManual.plotTopMeanFull()),
    Action("pe", "shows the effect of manual proposing", () => SubmissionTopManual.printEffectOfManual()),
    Action("rmsr", "Creates simple submission (e.g. all mean) and adds stepwise more and more real " +
      "values to see what could be the effect of training certain shop/items", () => StepwiseReal.runMeanWithStepwiseReal()),
  )

  private lazy val actionMap: Map[String, Action] = actions.map(a => (a.id, a)).toMap
  private lazy val actionsDesc: String = actions.map(a => f" - ${a.id}%-8s${a.desc}").mkString("\n")


  private def run(args: Array[String]): Unit = {
    if (args.length != 1) {
      println(s"ERROR: require argument 'action' not found'")
      println(s"possible actions:\n$actionsDesc")
    } else {
      val id = args(0)
      actionMap.get(id) match {
        case Some(value) =>
          println(s"-------------------------------------------------------------------------------------")
          println(s"Running '${value.id}'")
          println(s"${value.desc}")
          println(s"-------------------------------------------------------------------------------------")
          value.action()
        case None =>
          println(s"Error: Unknown action: '$id'")
          println(s"possible actions:\n$actionsDesc")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    run(args)
  }


}
