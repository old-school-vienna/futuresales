import TrainSet.{Norm, norm, normalize}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class TrainSetSuite extends AnyFunSuite with Matchers {

  val normData = Seq(
    (Seq(2.0, 4.0), 3.0, 1.0),
    (Seq(0.0, 4.0), 2.0, 2.0),
    (Seq(2.0, 4.1), 3.05, 1.05),
  )

  for (t <- normData) {
    test(s"norm $t") {
      val n = norm(t._1)
      n.mean mustBe t._2 +- 0.00001
      n.stdDeviation mustBe t._3 +- 0.00001
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
      normalize(t, n) mustBe e +- 0.0001
    }
  }


}
