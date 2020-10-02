import org.rogach.scallop.{ScallopConf, ScallopOption}
import Util.Situation

import scala.collection.immutable.ArraySeq

object Run {

  private case class Action(
                             id: String,
                             desc: String,
                             action: () => Unit
                           )

  private val actions = Seq(
    Action("printErrorTrain3", "Prints the error for the train3 submission", () => DfTrain.printErrorTrain3()),
    Action("printErrorTrain4", "Prints the error for the train3 submission", () => DfTrain.printErrorTrain4()),
    Action("printErrorAllReal", "Show that testing a submission with all real values leads to error zero", () => SubmissionTopManual.printErrorAllReal()),
    Action("plotErrorForMeanWithFactor", "Plot the error for mean submissions with factor", () => SubmissionTopManual.plotErrorForMeanWithFactor()),
    Action("submissionFileJustMean", "Create a kaggle submissin. All proposition mean of months", () => SubmissionMean.submissionFileJustMean()),
    Action("plotTopMeanFull", "Plots the monthly sales for the top shop/item(s)", () => SubmissionTopManual.plotTopMeanFull()),
    Action("printEffectOfManual", "shows the effect of manual proposing", () => SubmissionTopManual.printEffectOfManual()),
    Action("runMeanWithStepwiseReal", "Creates simple submission (e.g. all mean) and adds stepwise more and more real " +
      "values to see what could be the effect of training certain shop/items", () => StepwiseReal.runMeanWithStepwiseReal()),
  )
  private lazy val actionMap: Map[String, Action] = actions.map(a => (a.id, a)).toMap
  private lazy val actionsDesc: String = actions.map(a => s"\t${a.id} : ${a.desc}").mkString("\n")

  private class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val action: ScallopOption[String] = trailArg[String](descr = s"\n$actionsDesc")
    verify()
  }


  private def situationConverter(situation: ScallopOption[String]): Situation = {
    situation
      .map(s => if (s.toLowerCase() == "full") Situation.Full else Situation.Local)
      .getOrElse(Situation.Local)
  }

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args.toIndexedSeq)
    val id = conf.action.getOrElse("-")
    actionMap.get(id) match {
      case Some(value) =>
        println(s"-------------------------------------------------------------------------------------")
        println(s"Running '${value.id}'")
        println(s"${value.desc}")
        println(s"-------------------------------------------------------------------------------------")
        value.action()
      case None => println(s"Error: Unknown action: '$id'")
    }
  }


}
