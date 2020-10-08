import TrainSet._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

import scala.util.Random

class TrainSetSuite extends AnyFunSuite with Matchers {

  private val _e = 0.00001

  private val _trainSet1 = TrainSet(
    Seq(
      TrainSet.Row(Seq(0.0, 10.0, 5.0), 3.0),
      TrainSet.Row(Seq(1.0, 5.0, 15.0), 4.0),
      TrainSet.Row(Seq(2.0, 10.0, 30.0), 5.0),
    )
  )

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
    val ns = TrainSet.normSet(_trainSet1)
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
    test(s"normalize deNormalize value $v $n") {
      val v1 = normalize(v, n)
      deNormalize(v1, n) mustBe v +- _e
    }
  }
  for (v <- values; n <- norms) {
    test(s"deNormalize normalize value $v $n") {
      val v1 = deNormalize(v, n)
      normalize(v1, n) mustBe v +- _e
    }
  }

  test(s"normalize deNormalize train set 1") {
    val ns = normSet(_trainSet1)
    val ts0 = normalize(_trainSet1, ns)
    val ts1 = deNormalize(ts0, ns)

    ts1.rows.size mustBe _trainSet1.rows.size

    ts1.rows(0).predictors(0) mustBe _trainSet1.rows(0).predictors(0) +- _e
    ts1.rows(0).predictors(1) mustBe _trainSet1.rows(0).predictors(1) +- _e
    ts1.rows(0).predictors(2) mustBe _trainSet1.rows(0).predictors(2) +- _e

    ts1.rows(1).predictors(0) mustBe _trainSet1.rows(1).predictors(0) +- _e
    ts1.rows(1).predictors(1) mustBe _trainSet1.rows(1).predictors(1) +- _e
    ts1.rows(1).predictors(2) mustBe _trainSet1.rows(1).predictors(2) +- _e

    ts1.rows(2).predictors(0) mustBe _trainSet1.rows(2).predictors(0) +- _e
    ts1.rows(2).predictors(1) mustBe _trainSet1.rows(2).predictors(1) +- _e
    ts1.rows(2).predictors(2) mustBe _trainSet1.rows(2).predictors(2) +- _e
  }

  test("write read norm set") {
    val dataStdDeviation = Random.nextDouble() * 293.333
    val ns = NormSet(
      predictors = Seq(Norm(0, 1), Norm(10, 22.4)),
      data = Norm(344, dataStdDeviation))
    TrainSet.writeNormSet(ns, "test_norm_set")
    val ns1 = TrainSet.readNormSet("test_norm_set")

    ns1.predictors.size mustBe 2

    ns1.predictors.size mustBe 2
    ns1.predictors(0).mean mustBe 0.0 +- _e
    ns1.predictors(0).stdDeviation mustBe 1.0 +- _e
    ns1.predictors(1).mean mustBe 10.0 +- _e
    ns1.predictors(1).stdDeviation mustBe 22.4 +- _e

    ns1.data.mean mustBe 344.0 +- _e
    ns1.data.stdDeviation mustBe dataStdDeviation +- _e


  }

}
