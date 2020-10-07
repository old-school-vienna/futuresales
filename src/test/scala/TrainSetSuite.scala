import TrainSet.{Norm, deNormalize, norm, normalize}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class TrainSetSuite extends AnyFunSuite with Matchers {

  val _e = 0.00001

  val normData = Seq(
    (Seq(2.0, 4.0), 3.0, 1.0),
    (Seq(0.0, 4.0), 2.0, 2.0),
    (Seq(2.0, 4.1), 3.05, 1.05),
  )

  for (t <- normData) {
    test(s"norm $t") {
      val n = norm(t._1)
      n.mean mustBe t._2 +- _e
      n.stdDeviation mustBe t._3 +- _e
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
      normalize(t, n) mustBe e +- _e
    }
  }

  test("to norm set") {
    val ts = TrainSet(
      Seq(
        TrainSet.Row(Seq(0.0, 10.0, 5.0), 3.0),
        TrainSet.Row(Seq(1.0, 5.0, 15.0), 4.0),
        TrainSet.Row(Seq(2.0, 10.0, 30.0), 5.0),
      )
    )
    val ns = TrainSet.normSet(ts)
    ns.predictors.size mustBe 3
    ns.predictors(0).mean mustBe 1.0 +- _e
    ns.predictors(0).stdDeviation mustBe 0.81649 +- _e
    ns.predictors(1).mean mustBe 8.3333333 +- _e
    ns.predictors(1).stdDeviation mustBe 2.357022 +- _e
    ns.predictors(2).mean mustBe 16.666666 +- _e
    ns.predictors(2).stdDeviation mustBe 10.27402 +- _e
    ns.data.mean mustBe 4.0 +- _e
    ns.data.stdDeviation mustBe 0.81649 +- _e
  }

  val values = Seq(-10.0, -3.3333, 0.0, 2.2434, 12_293.9238742983)
  val norms = Seq(Norm(23.34, 3.4), Norm(1.2938, 393_287.4))
  for (v <- values; n <- norms) {
    test(s"normalize deNormalize $v $n ") {
      val v1 = normalize(v, n)
      deNormalize(v1, n) mustBe v +- _e
    }
  }
  for (v <- values; n <- norms) {
    test(s"deNormalize normalize $v $n ") {
      val v1 = deNormalize(v, n)
      normalize(v1, n) mustBe v +- _e
    }
  }
}
