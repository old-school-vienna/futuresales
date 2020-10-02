import org.rogach.scallop.{ScallopConf, ScallopOption}
import Util.Situation

object Run {


  private class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val situation: ScallopOption[String] = opt[String](descr = "Can be 'Full' or 'Local'")
    val action: ScallopOption[String] = trailArg[String](descr =
      "\n  - DfTrain3.printError: Prints the error for the train3 submission" +
      "\n  - foo: Another description")
    verify()
  }


  private def situationConverter(situation: ScallopOption[String]): Situation = {
    situation
      .map(s => if (s.toLowerCase() == "full") Situation.Full else Situation.Local)
      .getOrElse(Situation.Local)
  }

  def main(args: Array[String]) {
    val conf = new Conf(args)
    println("situation is: " + situationConverter(conf.situation))
    conf.action.getOrElse("-") match {
      case "DfTrain3.printError" => DfTrain3.printError()
      case action => println(s"Unknown action '$action'")
    }
  }


}
