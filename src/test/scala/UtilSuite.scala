import Util.Norm
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class UtilSuite extends AnyFunSuite with Matchers {

  test("range -1.5 1.5") {
    val s = Util.fromToStep(-1.5, 1.5, 0.2)
      .map(x => "%5.3f".format(x))
      .mkString(", ")
    s.mustBe("-1.500, -1.300, -1.100, -0.900, -0.700, -0.500, -0.300, -0.100, 0.100, 0.300, 0.500, 0.700, 0.900, 1.100, 1.300, 1.500")
  }

  test("range -1.5 1.7") {
    val s = Util.fromToStep(-1.5, 1.7, 0.21)
      .map(x => "%5.3f".format(x))
      .mkString(", ")
    s.mustBe("-1.500, -1.290, -1.080, -0.870, -0.660, -0.450, -0.240, -0.030, 0.180, 0.390, 0.600, 0.810, 1.020, 1.230, 1.440, 1.650")
  }

  val normData = Seq(
    (Seq(2.0, 4.0), 3.0, 1.0),
    (Seq(0.0, 4.0), 2.0, 2.0),
    (Seq(2.0, 4.1), 3.05, 1.05),
  )

  for (t <- normData) {
    test(s"norm $t") {
      val norm = Util.norm(t._1)
      norm.mean mustBe t._2 +- 0.00001
      norm.stdDeviation mustBe t._3 +- 0.00001
    }
  }

  val normalizeData = Seq(
    (1.0, Norm(0, 1), 1.0),
    (10.0, Norm(5, 5), 1.0),
    (0.0, Norm(5, 5), -1.0),
    (5.0, Norm(5, 5), 0.0),
    (1.0, Norm(5, 5), -0.8),
  )

  for ((t, n, e) <- normalizeData) {
    test(s"normalize $t $n $e") {
      Util.normalize(t, n) mustBe e +- 0.0001
    }
  }


}
