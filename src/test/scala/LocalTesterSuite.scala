import Util.createSubmission
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class LocalTesterSuite extends AnyFunSuite with Matchers {

  Seq(
    (213908, 45, 18975),
    (213909, 45, 14153),
    (213910, 45, 14899),
    (213911, 45, 9199),
  ).foreach { case (id, shopId, itemId) =>
    test(s"mapping shop item id to submission Id $id") {
      Util.submissionIdToShopItemId(id) mustBe ShopItemId(shopId, itemId)
      Util.shopItemIdToSubmissionId(ShopItemId(shopId = shopId, itemId = itemId)) mustBe Some(id)
    }
  }

  test("25 20949 on 461.0 must be 0.0") {
    val id = Util.shopItemIdToSubmissionId(ShopItemId(25, 20949)).getOrElse(-1)
    LocalTester.test(Seq(SubmissionDs(id, 461.0))) mustBe 0.0 +- 0.0001
  }
  test("31 20949 on 431.0 must be 0.0") {
    val id = Util.shopItemIdToSubmissionId(ShopItemId(31, 20949)).getOrElse(-1)
    LocalTester.test(Seq(SubmissionDs(id, 431.0))) mustBe 0.0 +- 0.0001
  }
  test("31 20949 on 432.0 must be 1.0") {
    val id = Util.shopItemIdToSubmissionId(ShopItemId(31, 20949)).getOrElse(-1)
    LocalTester.test(Seq(SubmissionDs(id, 432.0))) mustBe 1.0 +- 0.0001
  }
  test("31 20949 on 433.0 must be 4.0") {
    val id = Util.shopItemIdToSubmissionId(ShopItemId(31, 20949)).getOrElse(-1)
    LocalTester.test(Seq(SubmissionDs(id, 433.0))) mustBe 4.0 +- 0.0001
  }
  test("31 20949 on 429.0 must be 4.0") {
    val id = Util.shopItemIdToSubmissionId(ShopItemId(31, 20949)).getOrElse(-1)
    LocalTester.test(Seq(SubmissionDs(id, 429.0))) mustBe 4.0 +- 0.0001
  }
  test("mean of multiple submissions") {
    val submissions = Seq(
      SubmissionDs(Util.shopItemIdToSubmissionId(ShopItemId(31, 20949)).getOrElse(-1), 429.0),
      SubmissionDs(Util.shopItemIdToSubmissionId(ShopItemId(25, 20949)).getOrElse(-1), 464.0),
    )
    LocalTester.test(submissions) mustBe 6.5 +- 0.0001
  }
  test("all real must be 0.0") {
    val truth = LocalTester.truthMap

    def proposeReal(shopItemId: ShopItemId): Double = {
      val submissionId = Util.shopItemIdToSubmissionId(shopItemId)
      submissionId.map(sid => truth.getOrElse(sid, 0.0)).getOrElse(0.0)
    }

    val submission = createSubmission(proposeReal)
    val result = LocalTester.test(submission)
    result mustBe 0.0 +- 0.0001

  }

}
