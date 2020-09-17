import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class LocalTesterSuite extends AnyFunSuite with Matchers {

  test("visualize truth") {
    LocalTester.truthMap.toSeq.sortBy(_._1).take(100).foreach(println(_))
  }

  test("ID 24 to 2.0 must be 0") {
    LocalTester.test(Seq(SubmissionDs(24, 2.0))) mustBe 0.0 +- 0.0001
  }
  test("ID 24 to 4.0 must be 4") {
    LocalTester.test(Seq(SubmissionDs(24, 4.0))) mustBe 4.0 +- 0.0001
  }
  test("ID 24 to 0.0 must be 4") {
    LocalTester.test(Seq(SubmissionDs(24, 0.0))) mustBe 4.0 +- 0.0001
  }
  test("ID 30 to 0.0 must be 4") {
    LocalTester.test(Seq(SubmissionDs(30, 0.0))) mustBe 4.0 +- 0.0001
  }
  test("ID 24 and 30 to 0.0 must be 4") {
    val vs = Seq(
      SubmissionDs(24, 0.0),
      SubmissionDs(30, 0.0),
    )
    LocalTester.test(vs) mustBe 4.0 +- 0.0001
  }

  test("ID 0 to 2.4") {
    LocalTester.test(Seq(SubmissionDs(0, 2.4))) mustBe 0.16 +- 0.0001
  }
  test("ID 5 to 2.4") {
    LocalTester.test(Seq(SubmissionDs(5, 2.4))) mustBe 0.36 +- 0.0001
  }
  test("ID 200 to 2.4") {
    LocalTester.test(Seq(SubmissionDs(200, 2.4))) mustBe 5.76 +- 0.0001
  }
  test("ID 0,5 to 2.4") {
    val submissions = Seq(
      SubmissionDs(0, 2.4),
      SubmissionDs(5, 2.4),
    )
    LocalTester.test(submissions) mustBe 0.26 +- 0.0001
  }
}
